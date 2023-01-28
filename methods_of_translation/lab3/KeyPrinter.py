from gen.Var4Lexer import Var4Lexer
from gen.Var4Parser import Var4Parser
from gen.Var4Listener import Var4Listener


class KeyPrinter(Var4Listener):

    def __init__(self, to_file="output.c"):
        self._writer = open(to_file, 'w')
        self._indent = 0
        self.vars = dict()

    def __del__(self):
        self._writer.close()

    def _write(self, s):
        self._writer.write(s)

    def _get_spaces(self):
        return ' ' * self._indent * 4

    def enterStart(self, ctx: Var4Parser.StartContext):
        self._write("#include <stdio.h>\n\nint main() {\n")
        self._indent += 1

    def exitStart(self, ctx: Var4Parser.StartContext):
        self._write(self._get_spaces() + "return 0;\n}\n")

    def enterLine(self, ctx: Var4Parser.LineContext):
        self._write(self._get_spaces())

    def exitLine(self, ctx: Var4Parser.LineContext):
        if \
                ctx.SPS() \
                or ctx.if_() \
                or ctx.while_() \
                or (not ctx.children or len(ctx.children) == 1 and ctx.newline()):
            self._write("\n")
        else:
            self._write(";\n")

    def enterLastline(self, ctx: Var4Parser.LastlineContext):
        self.enterLine(ctx)

    def exitLastline(self, ctx: Var4Parser.LastlineContext):
        self.exitLine(ctx)

    def enterExpr(self, ctx: Var4Parser.ExprContext):
        if \
                not ctx.func() \
                and not isinstance(ctx.parentCtx, Var4Parser.ExprContext) \
                and not isinstance(ctx.parentCtx, Var4Parser.FuncContext) \
                and not isinstance(ctx.parentCtx, Var4Parser.IfContext) \
                and not isinstance(ctx.parentCtx, Var4Parser.ElifContext) \
                and not isinstance(ctx.parentCtx, Var4Parser.WhileContext):
            self._write(ctx.getText())

    def exitExpr(self, ctx: Var4Parser.ExprContext):
        pass

    def enterBlock(self, ctx: Var4Parser.BlockContext):
        self._write("{\n")
        self._indent += 1

    def exitBlock(self, ctx: Var4Parser.BlockContext):
        ks = [k for k, v in self.vars.items() if v == self._indent]
        for k in ks:
            self.vars.pop(k)
        self._indent -= 1
        self._write(self._get_spaces() + "}")

    def enterAssign(self, ctx: Var4Parser.AssignContext):
        if ctx.name().getText() not in self.vars:
            self._write("int ")
            self.vars[ctx.name().getText()] = self._indent
            self._write(ctx.name().getText() + ' = ')
            if ctx.MODONE():
                self._write(ctx.MODONE().getText()[0] + '1')
        elif ctx.EQ():
            self._write(ctx.name().getText() + ' = ')
        elif ctx.MOD():
            # self._write(ctx.name().getText() + ' = ' + ctx.name().getText() + f' {ctx.MOD().getText()[0]} ')
            self._write(ctx.name().getText() + f' {ctx.MOD().getText()} ')
        elif ctx.MODONE():
            self._write(ctx.name().getText() + ctx.MODONE().getText())

    def exitAssign(self, ctx: Var4Parser.AssignContext):
        pass

    def enterFunc(self, ctx: Var4Parser.FuncContext):
        self._write(ctx.getText())

    def exitFunc(self, ctx: Var4Parser.FuncContext):
        pass

    def enterName(self, ctx: Var4Parser.NameContext):
        pass

    def exitName(self, ctx: Var4Parser.NameContext):
        pass

    def enterInt(self, ctx: Var4Parser.IntContext):
        pass

    def exitInt(self, ctx: Var4Parser.IntContext):
        pass

    def enterIf(self, ctx: Var4Parser.IfContext):
        self._write(f"if ({ctx.expr().getText()}) ")

    def exitIf(self, ctx: Var4Parser.IfContext):
        pass

    def enterElif(self, ctx: Var4Parser.ElifContext):
        self._write(f" else if ({ctx.expr().getText()}) ")

    def exitElif(self, ctx: Var4Parser.ElifContext):
        pass

    def enterElse(self, ctx:Var4Parser.ElseContext):
        self._write(f" else ")

    def exitElse(self, ctx:Var4Parser.ElseContext):
        pass

    def enterWhile(self, ctx: Var4Parser.WhileContext):
        self._write(f"while ({ctx.expr().getText()}) ")

    def exitWhile(self, ctx: Var4Parser.WhileContext):
        pass

    def enterOp(self, ctx: Var4Parser.OpContext):
        pass

    def exitOp(self, ctx: Var4Parser.OpContext):
        pass
