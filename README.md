# Xenon ATC-X

## 简介

Xenon 是某大学编译原理课程的作业，其目标是把 SysY 编译为 RISC-V 汇编。SysY 是 C 语言的一个子集。

Xenon 已被废弃，Xenon ATC-X 是其后继项目。ATC-X = **A**dvanced **T**echnology **C**omplier-**X**（先进技术编译器-X）。除标准 SysY 语言外，ATC-X 还支持一些语言扩展：

1. 二进制整数字面量，如：
    ```c
    int a = 0b11011111101010010; // a = 114514
    ```
2. 自定义中缀运算符，例如：
    ```c
    int foo(int x) { /* do something */ }
    int foo2(int x) { /* do something */ }
    int bar(int x, int y) { /* do something */ }
    int main()
    {
        int x = 2, y = 2;
        x ~bar~ y;            // 等价于 bar(x, y);
    }
    ```
3. 管道运算符与成员访问运算符，例如：
    ```c
    int foo(int x) { /* do something */ }
    int foo2(int x) { /* do something */ }
    int bar(int x, int y) { /* do something */ }
    int main()
    {
        int x = 2, y = 2;
        x |> foo |> foo2;     // 等价于 foo2(foo(x));
        x |> foo() |> bar(y); // 等价于 bar(y, foo(x));
        bar(x) <| y;          // 等价于 bar(x, y);
        ((x.bar(y)) ~bar~ 114)
          |> foo;             // 等价于 foo(bar(bar(x, y), 114));
    }
    ```
4. 函数声明语法，允许在函数签名中不指明参数名称，对于没有参数的函数也允许使用 `void` 为参数列表。例如：
    ```c
    int foo(int);
    int bar(int, int);
    int main(void) { return foo(bar(114, 514)); }
    int bar(int x, int) { /* do something */ }
    ```
5. 各类位运算符，如 `<<`、`|`、`&` 和 `~`。
6. 各类复合赋值运算符，如 `+=`，`*=`。
7. 赋值运算符返回左值，这意味着可以写 `x = y = z;` 这种代码。
8. 自增自减运算符，其中前缀自增自减返回左值。例如 `++x = 1;`
9. 以常量下标取得的常量数组元素可以参与编译期常量表达式求值，例如：
    ```c
    int main()
    {
        const int a[1] = {10};
        const int x = a[0]; // x == 10
        int y[a[0]]; // y 的类型是 int[10]
        return 0;
    }
    ```

更详细的介绍，请见[《关于 Xenon（ATC-X） 内部实现的注释》](doc/xenon.tex)。

## 构建

构建编译器需要 Rust 工具链。构建文档需要 LuaLaTeX。

### Windows

`build.ps1` 是一个 PowerShell 编写的构建脚本。你可以：

- 运行 `.\build` 或 `.\build xenon` 以构建 Xenon ATC-X 编译器。
- 如果你的系统中安装了所需字体，运行 `.\build doc` 构建文档。
    这些字体是：
    - Source Han Serif SC（Regular 和 Bold）
    - Source Han Sans SC（Regular 和 Bold）
    - STKaiti
    - Noto Serif（Regular、Italic、Bold 和 Bold Italic）
    - Source Code Pro（Regular、Italic、Bold 和 Bold Italic）
    - Source Sans 3（Regular、Italic、Bold 和 Bold Italic）
- 否则，运行 `.\build doc-default-fonts` 构建文档。
- 运行 `.\build test-koopa` 测试 Koopa IR 生成。
- 运行 `.\build test-risc-v` 测试目标代码生成。
- 运行 `.\build all` 同时构建编译器和文档。

### 其他操作系统

- 运行 `cargo build -r` 以构建 Xenon ATC-X 编译器。
- 如果你的系统中安装了所需字体，运行：
    ```bash
    cd doc
    lualatex -shell-escape doc.tex
    ```
    构建文档
- 否则，运行
    ```bash
    cd doc
    lualatex -shell-escape "\NewDocumentCommand{\DefaultFonts}{}{}\input{doc.tex}"
    ```
    构建文档
