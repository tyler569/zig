const std = @import("../std.zig");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ast = std.zig.ast;
const Node = ast.Node;
const Tree = ast.Tree;
const Error = ast.Error;
const TokenIndex = ast.TokenIndex;
const Token = std.zig.Token;
const TokenIterator = Tree.TokenList.Iterator;

pub fn parse(allocator: *Allocator, source: []const u8) !Tree {
    var tree_arena = std.heap.ArenaAllocator.init(allocator);
    errdefer tree_arena.deinit();
    const arena = &tree_arena.allocator;

    var token_list = Tree.TokenList.init(arena);
    var tokenizer = std.zig.Tokenizer.init(source);
    while (true) {
        const tree_token = try token_list.addOne();
        tree_token.* = tokenizer.next();
        if (tree_token.id == Token.Id.Eof) break;
    }
    var it = token_list.iterator(0);

    var tree = Tree{
        .source = source,
        .root_node = undefined,
        .tokens = token_list,
        .errors = Tree.ErrorList.init(arena),
        // TODO: Remove (not used/needed anywhere)
        .arena_allocator = undefined,
    };

    tree.root_node = try parseRoot(arena, &it, &tree);

    return tree;
}

// Root <- skip ContainerMembers eof
fn parseRoot(arena: *Allocator, it: *TokenIterator, tree: *Tree) !*Node.Root {
    const node = try arena.create(Node.Root);
    node.* = Node.Root{
        .base = Node{ .id = Node.Id.Root },
        .decls = undefined,
        .doc_comments = null,
        .shebang = null,
        .eof_token = undefined,
    };
    node.decls = try parseContainerMembers(arena, it, tree);

    return node;
}

// ContainerMembers
//     <- TestDecl ContainerMembers
//      / TopLevelComptime ContainerMembers
//      / KEYWORD_pub? TopLevelDecl ContainerMembers
//      / KEYWORD_pub? ContainerField COMMA ContainerMembers
//      / KEYWORD_pub? ContainerField
//      /
fn parseContainerMembers(arena: *Allocator, it: *TokenIterator, tree: *Tree) !Node.Root.DeclList {
    var list = Node.Root.DeclList.init(arena);

    while (true) {
        if (try parseTestDecl(arena, it, tree)) |node| {
            try list.push(node);
            continue;
        }

        if (try parseTopLevelComptime(arena, it, tree)) |node| {
            try list.push(node);
            continue;
        }

        const visibility_token = eatToken(it, Token.Id.Keyword_pub);

        if (try parseTopLevelDecl(arena, it, tree, visibility_token)) |node| {
            try list.push(node);
            continue;
        }

        if (try parseContainerField(arena, it, tree)) |node| {
            node.cast(Node.StructField).?.visib_token = visibility_token;
            try list.push(node);
            if (eatToken(it, Token.Id.Comma)) |_| continue else break;
        }

        if (visibility_token == null) break;

        try tree.errors.push(Error{
            .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
        });
        break;
    }

    return list;
}

// TestDecl <- KEYWORD_test STRINGLITERAL Block
fn parseTestDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const test_token = eatToken(it, Token.Id.Keyword_test) orelse return null;
    const name_token = (try expectToken(it, tree, Token.Id.StringLiteral)) orelse return null;
    const block_node = (try expectNode(
        arena,
        it,
        tree,
        parseBlock,
        Error{ .ExpectedLBrace = Error.ExpectedLBrace{ .token = it.peek().?.start } },
    )) orelse return null;
    // TODO: deal with MultilineStringLiteralLine
    const name_node = try arena.create(Node.StringLiteral);
    name_node.* = Node.StringLiteral{
        .base = Node{ .id = .StringLiteral },
        .token = name_token,
    };

    const test_node = try arena.create(Node.TestDecl);
    test_node.* = Node.TestDecl{
        .base = Node{ .id = Node.Id.TestDecl },
        .doc_comments = null,
        .test_token = test_token,
        .name = &name_node.base,
        .body_node = block_node,
    };
    return &test_node.base;
}

// TopLevelComptime <- KEYWORD_comptime BlockExpr
fn parseTopLevelComptime(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const tok = eatToken(it, Token.Id.Keyword_comptime) orelse return null;
    const block_node = (try expectNode(arena, it, tree, parseBlockExpr, Error{
        .ExpectedLabelOrLBrace = Error.ExpectedLabelOrLBrace{ .token = it.peek().?.start },
    })) orelse return null;

    const comptime_node = try arena.create(Node.Comptime);
    comptime_node.* = Node.Comptime{
        .base = Node{ .id = Node.Id.Comptime },
        .doc_comments = null,
        .comptime_token = tok,
        .expr = block_node,
    };
    return &comptime_node.base;
}

// TopLevelDecl
//     <- (KEYWORD_export / KEYWORD_extern STRINGLITERAL? / KEYWORD_inline)? FnProto (SEMICOLON / Block)
//      / (KEYWORD_export / KEYWORD_extern STRINGLITERAL?)? KEYWORD_threadlocal? VarDecl
//      / KEYWORD_use Expr SEMICOLON
fn parseTopLevelDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree, vis: ?TokenIndex) !?*Node {
    const first = eatToken(it, Token.Id.Keyword_export) orelse
        eatToken(it, Token.Id.Keyword_extern) orelse
        eatToken(it, Token.Id.Keyword_inline);
    if (first) |keyword| {
        const keyword_id = tree.tokens.at(keyword).id;

        const extern_export_token = if (keyword_id == Token.Id.Keyword_export) keyword else null;
        const lib_name = blk: {
            if (keyword_id == Token.Id.Keyword_extern) {
                break :blk (try parseStringLiteral(arena, it, tree)) orelse null;
            } else break :blk null;
        };

        if (try parseFnProto(arena, it, tree)) |node| {
            const fn_node = node.cast(Node.FnProto).?;
            fn_node.visib_token = vis;
            fn_node.extern_export_inline_token = first;
            fn_node.lib_name = lib_name;

            if (eatToken(it, Token.Id.Semicolon)) |_| return node;
            if (try parseBlock(arena, it, tree)) |body_node| {
                fn_node.body_node = body_node;
                return node;
            }

            try tree.errors.push(Error{
                .ExpectedSemiOrLBrace = Error.ExpectedSemiOrLBrace{ .token = it.peek().?.start },
            });
            return null;
        }

        if (keyword_id == Token.Id.Keyword_inline) {
            try tree.errors.push(Error{
                .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
            });
            return null;
        }

        const thread_local_token = eatToken(it, Token.Id.Keyword_threadlocal);
        if (try parseVarDecl(arena, it, tree)) |node| {
            var var_decl = node.cast(Node.VarDecl).?;
            var_decl.*.doc_comments = null;
            var_decl.*.visib_token = vis;
            var_decl.*.thread_local_token = thread_local_token;
            var_decl.*.comptime_token = null;
            var_decl.*.extern_export_token = extern_export_token;
            var_decl.*.lib_name = lib_name;
            return node;
        }

        try tree.errors.push(Error{
            .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
        });
        return null;
    }

    const use_token = eatToken(it, Token.Id.Keyword_use) orelse return null;
    const use_node = (try parseUse(arena, it, tree)) orelse return null;
    const expr_node = (try expectNode(arena, it, tree, parseExpr, Error{
        .ExpectedExpr = Error.ExpectedExpr{ .token = it.peek().?.start },
    })) orelse return null;
    const semicolon_token = (try expectToken(it, tree, Token.Id.Semicolon)) orelse return null;
    const use_node_raw = use_node.cast(Node.Use).?;
    use_node_raw.*.visib_token = vis;
    use_node_raw.*.use_token = use_token;
    use_node_raw.*.expr = expr_node;
    use_node_raw.*.semicolon_token = semicolon_token;

    return use_node;
}

// FnProto <- FnCC? KEYWORD_fn IDENTIFIER? LPAREN ParamDeclList RPAREN ByteAlign? LinkSection? EXCLAMATIONMARK? (KEYWORD_var / TypeExpr)
fn parseFnProto(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const cc_token = parseFnCC(arena, it, tree);
    const fn_token = eatToken(it, Token.Id.Keyword_fn) orelse return null;
    const name_token = eatToken(it, Token.Id.Identifier);
    const lparen = (try expectToken(it, tree, Token.Id.LParen)) orelse return null;
    const params = (try parseParamDeclList(arena, it, tree)) orelse {
        try tree.errors.push(Error{
            .ExpectedParamList = Error.ExpectedParamList{ .token = it.peek().?.start },
        });
        return null;
    };
    const rparen = (try expectToken(it, tree, Token.Id.RParen)) orelse return null;
    const alignment_node = try parseByteAlign(arena, it, tree);
    const link_section_node = try parseLinkSection(arena, it, tree);
    const exclamation_token = eatToken(it, Token.Id.Bang);

    // (KW_var / TypeExpr)

    const fn_proto_node = try arena.create(Node.FnProto);
    fn_proto_node.* = Node.FnProto{
        .base = Node{ .id = Node.Id.FnProto },
        .doc_comments = null,
        .visib_token = null,
        .fn_token = fn_token,
        .name_token = name_token,
        .params = params,
        .return_type = undefined, // TODO ReturnType
        .var_args_token = undefined, // TODO ?TokenIndex
        .extern_export_inline_token = null,
        .cc_token = cc_token,
        .async_attr = null,
        .body_node = null,
        .lib_name = null,
        .align_expr = null,
        .section_expr = null,
    };

    return &fn_proto_node.base;
}

// VarDecl <- (KEYWORD_const / KEYWORD_var) IDENTIFIER (COLON TypeExpr)? ByteAlign? LinkSection? (EQUAL Expr)? SEMICOLON
fn parseVarDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const mut_token = eatToken(it, Token.Id.Keyword_const) orelse
        eatToken(it, Token.Id.Keyword_var) orelse
        return null;

    const name_token = (try expectToken(it, tree, Token.Id.Identifier)) orelse return null;
    const type_node = blk: {
        if (eatToken(it, Token.Id.Colon)) |_| {
            break :blk (try expectNode(arena, it, tree, parseTypeExpr, Error{
                .ExpectedTypeExpr = Error.ExpectedTypeExpr{ .token = it.peek().?.start },
            })) orelse return null;
        } else break :blk null;
    };
    const align_node = try parseByteAlign(arena, it, tree);
    const section_node = try parseLinkSection(arena, it, tree);
    const eq_token = eatToken(it, Token.Id.Equal);
    const init_node = blk: {
        if (eq_token) |_| {
            break :blk (try expectNode(arena, it, tree, parseExpr, Error{
                .ExpectedExpr = Error.ExpectedExpr{ .token = it.peek().?.start },
            })) orelse return null;
        } else break :blk null;
    };
    const semicolon_token = (try expectToken(it, tree, Token.Id.Semicolon)) orelse return null;

    const node = try arena.create(Node.VarDecl);
    node.* = Node.VarDecl{
        .base = Node{ .id = Node.Id.VarDecl },
        .mut_token = mut_token,
        .name_token = name_token,
        .eq_token = eq_token orelse 0,
        .type_node = type_node,
        .align_node = align_node,
        .section_node = section_node,
        .init_node = init_node,
        .semicolon_token = semicolon_token,
        // set by caller
        .doc_comments = null,
        .visib_token = null,
        .thread_local_token = null,
        .comptime_token = null,
        .extern_export_token = null,
        .lib_name = null,
    };

    return &node.base;
}

// ContainerField <- IDENTIFIER (COLON TypeExpr)? (EQUAL Expr)?
fn parseContainerField(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const name_token = eatToken(it, Token.Id.Identifier) orelse return null;
    const type_expr = blk: {
        if (eatToken(it, Token.Id.Colon)) |_| {
            break :blk (try expectNode(arena, it, tree, parseTypeExpr, Error{
                .ExpectedTypeExpr = Error.ExpectedTypeExpr{ .token = it.peek().?.start },
            })) orelse return null;
        } else break :blk null;
    };
    const default_value = blk: {
        if (eatToken(it, Token.Id.Equal)) |_| {
            break :blk (try expectNode(arena, it, tree, parseExpr, Error{
                .ExpectedExpr = Error.ExpectedExpr{ .token = it.peek().?.start },
            }));
        } else break :blk null;
    };
    _ = default_value; // TODO: use default value when ast.Node.StructField supports it

    const node = try arena.create(Node.StructField);
    node.* = Node.StructField{
        .base = Node{ .id = Node.Id.StructField },
        .name_token = name_token,
        .type_expr = type_expr orelse undefined,
        // set by caller
        .doc_comments = null,
        .visib_token = null,
    };

    return &node.base;
}

// Statement
//     <- KEYWORD_comptime? VarDecl
//      / KEYWORD_comptime BlockExprStatement
//      / KEYWORD_suspend (SEMICOLON / BlockExprStatement)
//      / KEYWORD_defer BlockExprStatement
//      / KEYWORD_errdefer BlockExprStatement
//      / IfStatement
//      / LabeledStatement
//      / SwitchExpr
//      / AssignExpr SEMICOLON
fn parseStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const comptime_token = eatToken(it, Token.Id.Keyword_comptime);

    const var_decl_node = try parseVarDecl(arena, it, tree);
    if (var_decl_node) |node| {
        const var_decl = node.cast(Node.VarDecl).?;
        var_decl.comptime_token = comptime_token;
        return node;
    }

    if (comptime_token) |token| {
        const block_expr = (try expectNode(arena, it, tree, parseBlockExprStatement, Error{
            .ExpectedBlockOrAssignment = Error.ExpectedBlockOrAssignment{ .token = it.peek().?.start },
        })) orelse return null;

        const node = try arena.create(Node.Comptime);
        node.* = Node.Comptime{
            .base = Node{ .id = Node.Id.Comptime },
            .doc_comments = null,
            .comptime_token = token,
            .expr = block_expr,
        };
        return &node.base;
    }

    if (eatToken(it, Token.Id.Keyword_suspend)) |suspend_token| {
        const semicolon = eatToken(it, Token.Id.Semicolon);

        const body_node = if (semicolon == null) blk: {
            break :blk (try expectNode(arena, it, tree, parseBlockExprStatement, Error{
                // TODO: expected block or expression
                .ExpectedBlockOrAssignment = Error.ExpectedBlockOrAssignment{ .token = it.peek().?.start },
            })) orelse return null;
        } else null;

        const node = try arena.create(Node.Suspend);
        node.* = Node.Suspend{
            .base = Node{ .id = Node.Id.Suspend },
            .suspend_token = suspend_token,
            .body = body_node,
        };
        return &node.base;
    }

    const defer_token = eatToken(it, Token.Id.Keyword_defer) orelse eatToken(it, Token.Id.Keyword_errdefer);
    if (defer_token) |token| {
        const expr_node = (try expectNode(arena, it, tree, parseBlockExprStatement, Error{
            // TODO: expected block or expression
            .ExpectedBlockOrAssignment = Error.ExpectedBlockOrAssignment{ .token = it.peek().?.start },
        })) orelse return null;
        const node = try arena.create(Node.Defer);
        node.* = Node.Defer{
            .base = Node{ .id = Node.Id.Defer },
            .defer_token = token,
            .expr = expr_node,
        };
        return &node.base;
    }

    if (try parseIfStatement(arena, it, tree)) |node| return node;
    if (try parseLabeledStatement(arena, it, tree)) |node| return node;
    if (try parseSwitchExpr(arena, it, tree)) |node| return node;
    if (try parseAssignExpr(arena, it, tree)) |node| {
        _ = (try expectToken(it, tree, Token.Id.Semicolon)) orelse return null;
        return node;
    }

    return null;
}

// IfStatement
//     <- IfPrefix BlockExpr ( KEYWORD_else Payload? Statement )?
//      / IfPrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
fn parseIfStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const if_node = if (try parseIfPrefix(arena, it, tree)) |node| node.cast(Node.If).? else return null;
    const block_expr = (try parseBlockExpr(arena, it, tree));
    const assign_expr = if (block_expr == null) blk: {
        break :blk (try parseAssignExpr(arena, it, tree)) orelse null;
    } else null;
    const semicolon = if (assign_expr != null) eatToken(it, Token.Id.Semicolon) else null;

    const else_node = if (semicolon != null) blk: {
        const else_token = eatToken(it, Token.Id.Keyword_else) orelse break :blk null;
        const payload = try parsePayload(arena, it, tree);
        const else_body = (try expectNode(arena, it, tree, parseStatement, Error{
            .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
        })) orelse return null;

        const node = try arena.create(Node.Else);
        node.* = Node.Else{
            .base = Node{ .id = Node.Id.Else },
            .else_token = else_token,
            .payload = payload,
            .body = else_body,
        };

        break :blk node;
    } else null;

    if (block_expr) |body| {
        if_node.body = body;
        if_node.@"else" = else_node;
        return &if_node.base;
    }

    if (assign_expr) |body| {
        if_node.body = body;
        if (semicolon != null) return &if_node.base;
        if (else_node != null) {
            if_node.@"else" = else_node;
            return &if_node.base;
        }
        try tree.errors.push(Error{
            .ExpectedSemiOrElse = Error.ExpectedSemiOrElse{ .token = it.peek().?.start },
        });
    }

    return null;
}

// LabeledStatement <- BlockLabel? (Block / LoopStatement)
fn parseLabeledStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const label_token = parseBlockLabel(arena, it, tree);

    if (try parseBlock(arena, it, tree)) |node| {
        node.cast(Node.Block).?.label = label_token;
        return node;
    }

    if (try parseLoopStatement(arena, it, tree)) |node| {
        if (node.cast(Node.For)) |for_node| {
            for_node.label = label_token;
        } else if (node.cast(Node.While)) |while_node| {
            while_node.label = label_token;
        } else unreachable;
        return node;
    }

    return null;
}

// LoopStatement <- KEYWORD_inline? (ForStatement / WhileStatement)
fn parseLoopStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const inline_token = eatToken(it, Token.Id.Keyword_inline);

    if (try parseForStatement(arena, it, tree)) |node| {
        node.cast(Node.For).?.inline_token = inline_token;
        return node;
    }

    if (try parseWhileStatement(arena, it, tree)) |node| {
        node.cast(Node.While).?.inline_token = inline_token;
        return node;
    }

    return null;
}

// ForStatement
//     <- ForPrefix BlockExpr ( KEYWORD_else Statement )?
//      / ForPrefix AssignExpr ( SEMICOLON / KEYWORD_else Statement )
fn parseForStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const for_prefix = (try parseForPrefix(arena, it, tree)) orelse return null;

    if (try parseBlockExpr(arena, it, tree)) |block_expr_node| {
        if (eatToken(it, Token.Id.Keyword_else)) |_| {
            const statement_node = (try expectNode(arena, it, tree, parseStatement, Error{
                .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
            })) orelse return null;
            const for_node = try arena.create(Node.For);
            for_node.* = Node.For{
                .base = Node{ .id = Node.Id.For },
                .label = undefined, // TODO
                .inline_token = undefined, // TODO
                .for_token = undefined, // TODO
                .array_expr = undefined, // TODO
                .payload = undefined, // TODO
                .body = undefined, // TODO
                .@"else" = undefined, // TODO
            };

            return &for_node.base;
        }

        return block_expr_node;
    }

    return error.NotImplemented;
}

// WhileStatement
//     <- WhilePrefix BlockExpr ( KEYWORD_else Payload? Statement )?
//      / WhilePrefix AssignExpr ( SEMICOLON / KEYWORD_else Payload? Statement )
fn parseWhileStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BlockExprStatement
//     <- BlockExpr
//      / AssignExpr SEMICOLON
fn parseBlockExprStatement(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    if (try parseBlockExpr(arena, it, tree)) |node| return node;
    if (try parseAssignExpr(arena, it, tree)) |node| {
        _ = (try expectToken(it, tree, Token.Id.Semicolon)) orelse return null;
        return node;
    }
    return null;
}

// BlockExpr <- BlockLabel? Block
fn parseBlockExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    const label_token = parseBlockLabel(arena, it, tree) orelse return null;
    const block_node = (parseBlock(arena, it, tree) catch return error.TodoFixRecursion) orelse return null;
    block_node.cast(Node.Block).?.label = label_token;
    return block_node;
}

// AssignExpr <- Expr (AssignOp Expr)?
fn parseAssignExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// Expr <- KEYWORD_try* BoolOrExpr
fn parseExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return parsePrefixOpExpr(arena, it, tree, parseTry, parseBoolOrExpr);
}

// BoolOrExpr <- BoolAndExpr (KEYWORD_or BoolAndExpr)*
fn parseBoolOrExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BoolAndExpr <- CompareExpr (KEYWORD_and CompareExpr)*
fn parseBoolAndExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// CompareExpr <- BitwiseExpr (CompareOp BitwiseExpr)?
fn parseCompareExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BitwiseExpr <- BitShiftExpr (BitwiseOp BitShiftExpr)*
fn parseBitwiseExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BitShiftExpr <- AdditionExpr (BitShiftOp AdditionExpr)*
fn parseBitShiftExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AdditionExpr <- MultiplyExpr (AdditionOp MultiplyExpr)*
fn parseAdditionExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// MultiplyExpr <- PrefixExpr (MultiplyOp PrefixExpr)*
fn parseMultiplyExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PrefixExpr <- PrefixOp* PrimaryExpr
fn parsePrefixExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PrimaryExpr
//     <- AsmExpr
//      / IfExpr
//      / KEYWORD_break BreakLabel? Expr?
//      / KEYWORD_cancel Expr
//      / KEYWORD_comptime Expr
//      / KEYWORD_continue BreakLabel?
//      / KEYWORD_resume Expr
//      / KEYWORD_return Expr?
//      / BlockLabel? LoopExpr
//      / Block
//      / CurlySuffixExpr
fn parsePrimaryExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// IfExpr <- IfPrefix Expr (KEYWORD_else Payload? Expr)?
fn parseIfExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// Block <- LBRACE Statement* RBRACE
fn parseBlock(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    const lbrace = eatToken(it, Token.Id.LBrace) orelse return null;

    var statements = Node.Block.StatementList.init(arena);
    while (true) {
        const statement = (try parseStatement(arena, it, tree)) orelse break;
        try statements.push(statement);
    }

    const rbrace = (try expectToken(it, tree, Token.Id.RBrace)) orelse return null;

    const block_node = try arena.create(Node.Block);
    block_node.* = Node.Block{
        .base = Node{ .id = Node.Id.Block },
        .label = null, // set by caller
        .lbrace = lbrace,
        .statements = statements,
        .rbrace = rbrace,
    };

    return &block_node.base;
}

// LoopExpr <- KEYWORD_inline? (ForExpr / WhileExpr)
fn parseLoopExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ForExpr <- ForPrefix Expr (KEYWORD_else Expr)?
fn parseForExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// WhileExpr <- WhilePrefix Expr (KEYWORD_else Payload? Expr)?
fn parseWhileExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// CurlySuffixExpr <- TypeExpr InitList?
fn parseCurlySuffixExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// InitList
//     <- LBRACE FieldInit (COMMA FieldInit)* COMMA? RBRACE
//      / LBRACE Expr (COMMA Expr)* COMMA? RBRACE
//      / LBRACE RBRACE
fn parseInitList(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// TypeExpr <- PrefixTypeOp* ErrorUnionExpr
fn parseTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return parsePrefixOpExpr(arena, it, tree, parsePrefixTypeOp, parseErrorUnionExpr);
}

// ErrorUnionExpr <- SuffixExpr (EXCLAMATIONMARK TypeExpr)?
fn parseErrorUnionExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SuffixExpr
//     <- AsyncPrefix PrimaryTypeExpr SuffixOp* FnCallArguments
//      / PrimaryTypeExpr (SuffixOp / FnCallArguments)*
fn parseSuffixExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PrimaryTypeExpr
//     <- BUILTINIDENTIFIER FnCallArguments
//      / CHAR_LITERAL
//      / ContainerDecl
//      / ErrorSetDecl
//      / FLOAT
//      / FnProto
//      / GroupedExpr
//      / LabeledTypeExpr
//      / IDENTIFIER
//      / IfTypeExpr
//      / INTEGER
//      / KEYWORD_anyerror
//      / KEYWORD_comptime TypeExpr
//      / KEYWORD_error DOT IDENTIFIER
//      / KEYWORD_false
//      / KEYWORD_null
//      / KEYWORD_promise
//      / KEYWORD_true
//      / KEYWORD_undefined
//      / KEYWORD_unreachable
//      / STRINGLITERAL
//      / SwitchExpr
fn parsePrimaryTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ContainerDecl <- (KEYWORD_extern / KEYWORD_packed)? ContainerDeclAuto
fn parseContainerDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ErrorSetDecl <- KEYWORD_error LBRACE IdentifierList RBRACE
fn parseErrorSetDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// GroupedExpr <- LPAREN Expr RPAREN
fn parseGroupedExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// IfTypeExpr <- IfPrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
fn parseIfTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// LabeledTypeExpr
//     <- BlockLabel Block
//      / BlockLabel? LoopTypeExpr
fn parseLabeledTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// LoopTypeExpr <- KEYWORD_inline? (ForTypeExpr / WhileTypeExpr)
fn parseLoopTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ForTypeExpr <- ForPrefix TypeExpr (KEYWORD_else TypeExpr)?
fn parseForTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// WhileTypeExpr <- WhilePrefix TypeExpr (KEYWORD_else Payload? TypeExpr)?
fn parseWhileTypeExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SwitchExpr <- KEYWORD_switch LPAREN Expr RPAREN LBRACE SwitchProngList RBRACE
fn parseSwitchExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmExpr <- KEYWORD_asm KEYWORD_volatile? LPAREN STRINGLITERAL AsmOutput? RPAREN
fn parseAsmExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// TODO: EnumLiteral
fn parseEnumLiteral(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmOutput <- COLON AsmOutputList AsmInput?
fn parseAsmOutput(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmOutputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN (MINUSRARROW TypeExpr / IDENTIFIER) RPAREN
fn parseAsmOutputItem(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmInput <- COLON AsmInputList AsmClobbers?
fn parseAsmInput(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmInputItem <- LBRACKET IDENTIFIER RBRACKET STRINGLITERAL LPAREN Expr RPAREN
fn parseAsmInputItem(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsmClobbers <- COLON StringList
fn parseAsmClobbers(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BreakLabel <- COLON IDENTIFIER
fn parseBreakLabel(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BlockLabel <- IDENTIFIER COLON
fn parseBlockLabel(arena: *Allocator, it: *TokenIterator, tree: *Tree) ?TokenIndex {
    const token = eatToken(it, Token.Id.Identifier) orelse return null;
    _ = eatToken(it, Token.Id.Colon) orelse return null;
    return token;
}

// FieldInit <- DOT IDENTIFIER EQUAL Expr
fn parseFieldInit(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// WhileContinueExpr <- COLON LPAREN AssignExpr RPAREN
fn parseWhileContinueExpr(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// LinkSection <- KEYWORD_linksection LPAREN Expr RPAREN
fn parseLinkSection(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// FnCC
//     <- KEYWORD_nakedcc
//      / KEYWORD_stdcallcc
//      / KEYWORD_extern
//      / KEYWORD_async (LARROW TypeExpr RARROW)?
fn parseFnCC(arena: *Allocator, it: *TokenIterator, tree: *Tree) ?TokenIndex {
    // TODO
    return null;
}

// ParamDecl <- (KEYWORD_noalias / KEYWORD_comptime)? (IDENTIFIER COLON)? ParamType
fn parseParamDecl(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ParamType
//     <- KEYWORD_var
//      / DOT3
//      / TypeExpr
fn parseParamType(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// IfPrefix <- KEYWORD_if LPAREN Expr RPAREN PtrPayload?
fn parseIfPrefix(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const if_token = eatToken(it, Token.Id.Keyword_if) orelse return null;
    _ = (try expectToken(it, tree, Token.Id.LParen)) orelse return null;
    const condition = (try expectNode(arena, it, tree, parseExpr, Error{
        .ExpectedExpr = Error.ExpectedExpr{ .token = it.peek().?.start },
    })) orelse return null;
    _ = (try expectToken(it, tree, Token.Id.RParen)) orelse return null;
    const payload = try parsePtrPayload(arena, it, tree);

    const node = try arena.create(Node.If);
    node.* = Node.If{
        .base = Node{ .id = Node.Id.If },
        .if_token = if_token,
        .condition = condition,
        .payload = payload,
        .body = undefined, // set by caller
        .@"else" = null,
    };
    return &node.base;
}

// WhilePrefix <- KEYWORD_while LPAREN Expr RPAREN PtrPayload? WhileContinueExpr?
fn parseWhilePrefix(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ForPrefix <- KEYWORD_for LPAREN Expr RPAREN PtrIndexPayload
fn parseForPrefix(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// Payload <- PIPE IDENTIFIER PIPE
fn parsePayload(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PtrPayload <- PIPE ASTERISK? IDENTIFIER PIPE
fn parsePtrPayload(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PtrIndexPayload <- PIPE ASTERISK? IDENTIFIER (COMMA IDENTIFIER)? PIPE
fn parsePtrIndexPayload(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SwitchProng <- SwitchCase EQUALRARROW PtrPayload? AssignExpr
fn parseSwitchProng(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SwitchCase
//     <- SwitchItem (COMMA SwitchItem)* COMMA?
//      / KEYWORD_else
fn parseSwitchCase(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SwitchItem <- Expr (DOT3 Expr)?
fn parseSwitchItem(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AssignOp
//     <- ASTERISKEQUAL
//      / SLASHEQUAL
//      / PERCENTEQUAL
//      / PLUSEQUAL
//      / MINUSEQUAL
//      / LARROW2EQUAL
//      / RARROW2EQUAL
//      / AMPERSANDEQUAL
//      / CARETEQUAL
//      / PIPEEQUAL
//      / ASTERISKPERCENTEQUAL
//      / PLUSPERCENTEQUAL
//      / MINUSPERCENTEQUAL
//      / EQUAL
fn parseAssignOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// CompareOp
//     <- EQUALEQUAL
//      / EXCLAMATIONMARKEQUAL
//      / LARROW
//      / RARROW
//      / LARROWEQUAL
//      / RARROWEQUAL
fn parseCompareOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BitwiseOp
//     <- AMPERSAND
//      / CARET
//      / PIPE
//      / KEYWORD_orelse
//      / KEYWORD_catch Payload?
fn parseBitwiseOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// BitShiftOp
//     <- LARROW2
//      / RARROW2
fn parseBitShiftOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AdditionOp
//     <- PLUS
//      / MINUS
//      / PLUS2
//      / PLUSPERCENT
//      / MINUSPERCENT
fn parseAdditionOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// MultiplyOp
//     <- PIPE2
//      / ASTERISK
//      / SLASH
//      / PERCENT
//      / ASTERISK2
//      / ASTERISKPERCENT
fn parseMultiplyOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PrefixOp
//     <- EXCLAMATIONMARK
//      / MINUS
//      / TILDE
//      / MINUSPERCENT
//      / AMPERSAND
//      / KEYWORD_try
//      / KEYWORD_await
fn parsePrefixOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PrefixTypeOp
//     <- QUESTIONMARK
//      / KEYWORD_promise MINUSRARROW
//      / ArrayTypeStart (ByteAlign / KEYWORD_const / KEYWORD_volatile)*
//      / PtrTypeStart (KEYWORD_align LPAREN Expr (COLON INTEGER COLON INTEGER)? RPAREN / KEYWORD_const / KEYWORD_volatile)*
fn parsePrefixTypeOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// SuffixOp
//     <- LBRACKET Expr (DOT2 Expr?)? RBRACKET
//      / DOT IDENTIFIER
//      / DOTASTERISK
//      / DOTQUESTIONMARK
fn parseSuffixOp(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// AsyncPrefix <- KEYWORD_async (LARROW PrefixExpr RARROW)?
fn parseAsyncPrefix(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// FnCallArguments <- LPAREN ExprList RPAREN
fn parseFnCallArguments(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ArrayTypeStart <- LBRACKET Expr? RBRACKET
fn parseArrayTypeStart(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// PtrTypeStart
//     <- ASTERISK
//      / ASTERISK2
//      / PTRUNKNOWN
//      / PTRC
fn parsePtrTypeStart(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ContainerDeclAuto <- ContainerDeclType LBRACE ContainerMembers RBRACE
fn parseContainerDeclAuto(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ContainerDeclType
//     <- (KEYWORD_struct / KEYWORD_enum) (LPAREN Expr RPAREN)?
//      / KEYWORD_union (LPAREN (KEYWORD_enum (LPAREN Expr RPAREN)? / Expr) RPAREN)?
fn parseContainerDeclType(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// ByteAlign <- KEYWORD_align LPAREN Expr RPAREN
fn parseByteAlign(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    return error.NotImplemented;
}

// IdentifierList <- (IDENTIFIER COMMA)* IDENTIFIER?
fn parseIdentifierList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// SwitchProngList <- (SwitchProng COMMA)* SwitchProng?
fn parseSwitchProngList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// AsmOutputList <- (AsmOutputItem COMMA)* AsmOutputItem?
fn parseAsmOutputList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// AsmInputList <- (AsmInputItem COMMA)* AsmInputItem?
fn parseAsmInputList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// StringList <- (STRINGLITERAL COMMA)* STRINGLITERAL?
fn parseStringList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// ParamDeclList <- (ParamDecl COMMA)* ParamDecl?
fn parseParamDeclList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?Node.FnProto.ParamList {
    return error.NotImplemented;
}

// ExprList <- (Expr COMMA)* Expr?
fn parseExprList(arena: *Allocator, it: *TokenIterator, tree: *Tree) anyerror!?*Node {
    return error.NotImplemented;
}

// TODO: don't use anyerror
const ParseFn = fn (*Allocator, *TokenIterator, *Tree) anyerror!?*Node;

// Helper parsers not included in the grammar

// string literal or multiline string literal
fn parseStringLiteral(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    if (eatToken(it, Token.Id.StringLiteral)) |token| {
        const node = try arena.create(Node.StringLiteral);
        node.* = Node.StringLiteral{
            .base = Node{ .id = Node.Id.StringLiteral },
            .token = token,
        };
        return &node.base;
    }

    if (eatToken(it, Token.Id.MultilineStringLiteralLine)) |first_line| {
        const node = try arena.create(Node.MultilineStringLiteral);
        node.* = Node.MultilineStringLiteral{
            .base = Node{ .id = Node.Id.MultilineStringLiteral },
            .lines = Node.MultilineStringLiteral.LineList.init(arena),
        };
        try node.lines.push(first_line);
        while (eatToken(it, Token.Id.MultilineStringLiteralLine)) |line|
            try node.lines.push(line);

        return &node.base;
    }

    return null;
}

// try
fn parseTry(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const token = eatToken(it, Token.Id.Keyword_try) orelse return null;
    const node = try arena.create(Node.PrefixOp);
    node.* = Node.PrefixOp{
        .base = Node{ .id = Node.Id.PrefixOp },
        .op_token = token,
        .op = Node.PrefixOp.Op.Try,
        .rhs = undefined, // set by caller
    };
    return &node.base;
}

// use
fn parseUse(arena: *Allocator, it: *TokenIterator, tree: *Tree) !?*Node {
    const token = eatToken(it, Token.Id.Keyword_use) orelse return null;
    const node = try arena.create(Node.Use);
    node.* = Node.Use{
        .base = Node{ .id = Node.Id.PrefixOp },
        .doc_comments = null,
        .visib_token = null,
        .use_token = undefined,
        .expr = undefined,
        .semicolon_token = undefined,
    };
    return &node.base;
}

// Op* Child
fn parsePrefixOpExpr(
    arena: *Allocator,
    it: *TokenIterator,
    tree: *Tree,
    opParseFn: ParseFn,
    childParseFn: ParseFn,
    // TODO: don't use anyerror
) anyerror!?*Node {
    if (try opParseFn(arena, it, tree)) |op| {
        const child = (try expectNode(
            arena,
            it,
            tree,
            childParseFn,
            Error{
                .InvalidToken = Error.InvalidToken{ .token = it.peek().?.start },
            },
        )) orelse return null;
        op.cast(Node.PrefixOp).?.rhs = child;
        return op;
    }
    return childParseFn(arena, it, tree);
}

fn eatToken(it: *TokenIterator, id: Token.Id) ?TokenIndex {
    return if (it.peek().?.id == id) nextToken(it).index else null;
}

fn expectToken(it: *TokenIterator, tree: *Tree, id: Token.Id) !?TokenIndex {
    const token = nextToken(it);
    if (token.ptr.id != id) {
        try tree.errors.push(Error{
            .ExpectedToken = Error.ExpectedToken{ .token = token.index, .expected_id = id },
        });
        return null;
    }
    return token.index;
}

fn nextToken(it: *TokenIterator) AnnotatedToken {
    const tok = AnnotatedToken{
        .index = it.index,
        .ptr = it.next().?,
    };
    assert(tok.ptr.id != Token.Id.LineComment);

    while (true) : (_ = it.next()) {
        const next_tok = it.peek() orelse return tok;
        if (next_tok.id != Token.Id.LineComment) return tok;
    }
}

fn prevToken(it: *TokenIterator) void {
    while (true) {
        const prev_tok = it.prev() orelse return;
        if (prev_tok.id == Token.Id.LineComment) continue;
        return;
    }
}

const AnnotatedToken = struct {
    ptr: *Token,
    index: TokenIndex,
};

// TODO: don't use anyerror
fn expectNode(
    arena: *Allocator,
    it: *TokenIterator,
    tree: *Tree,
    parseFn: ParseFn,
    err: Error, // if parsing fails
) anyerror!?*Node {
    const node = try parseFn(arena, it, tree);
    if (node == null) try tree.errors.push(err);
    return node;
}

test "std.zig.parser" {
    _ = @import("parser_test.zig");
}
