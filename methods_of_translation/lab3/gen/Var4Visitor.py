# Generated from C:/Users/Legend/PycharmProjects/Translation_lab3/check_antlr\Var4.g4 by ANTLR 4.11.1
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .Var4Parser import Var4Parser
else:
    from Var4Parser import Var4Parser

# This class defines a complete generic visitor for a parse tree produced by Var4Parser.

class Var4Visitor(ParseTreeVisitor):

    # Visit a parse tree produced by Var4Parser#start.
    def visitStart(self, ctx:Var4Parser.StartContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#lines.
    def visitLines(self, ctx:Var4Parser.LinesContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#line.
    def visitLine(self, ctx:Var4Parser.LineContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#lastline.
    def visitLastline(self, ctx:Var4Parser.LastlineContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#expr.
    def visitExpr(self, ctx:Var4Parser.ExprContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#block.
    def visitBlock(self, ctx:Var4Parser.BlockContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#assign.
    def visitAssign(self, ctx:Var4Parser.AssignContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#func.
    def visitFunc(self, ctx:Var4Parser.FuncContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#name.
    def visitName(self, ctx:Var4Parser.NameContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#int.
    def visitInt(self, ctx:Var4Parser.IntContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#if.
    def visitIf(self, ctx:Var4Parser.IfContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#elif.
    def visitElif(self, ctx:Var4Parser.ElifContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#else.
    def visitElse(self, ctx:Var4Parser.ElseContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#while.
    def visitWhile(self, ctx:Var4Parser.WhileContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#op.
    def visitOp(self, ctx:Var4Parser.OpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Var4Parser#newline.
    def visitNewline(self, ctx:Var4Parser.NewlineContext):
        return self.visitChildren(ctx)



del Var4Parser