# Generated from C:/Users/Legend/PycharmProjects/Translation_lab4/g4\Grm2.g4 by ANTLR 4.11.1
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .Grm2Parser import Grm2Parser
else:
    from Grm2Parser import Grm2Parser

# This class defines a complete generic visitor for a parse tree produced by Grm2Parser.

class Grm2Visitor(ParseTreeVisitor):

    # Visit a parse tree produced by Grm2Parser#start.
    def visitStart(self, ctx:Grm2Parser.StartContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#choose_rule.
    def visitChoose_rule(self, ctx:Grm2Parser.Choose_ruleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#skip_rule.
    def visitSkip_rule(self, ctx:Grm2Parser.Skip_ruleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#non_term_rule.
    def visitNon_term_rule(self, ctx:Grm2Parser.Non_term_ruleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#args.
    def visitArgs(self, ctx:Grm2Parser.ArgsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#non_term_returns.
    def visitNon_term_returns(self, ctx:Grm2Parser.Non_term_returnsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#arg.
    def visitArg(self, ctx:Grm2Parser.ArgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#rightPart.
    def visitRightPart(self, ctx:Grm2Parser.RightPartContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#ruleToken.
    def visitRuleToken(self, ctx:Grm2Parser.RuleTokenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by Grm2Parser#param.
    def visitParam(self, ctx:Grm2Parser.ParamContext):
        return self.visitChildren(ctx)



del Grm2Parser