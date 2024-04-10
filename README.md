# Xenon ATC-X

## 简介

Xenon 是某大学编译原理课程的作业，其目标是把 SysY 编译为 RISC-V 汇编。SysY 是 C 语言的一个子集。

Xenon 已被废弃，Xenon ATC-X 是其后继项目。ATC-X = Advanced Technology Complier-X（先进技术编译器-X）。除标准 SysY 语言外，ATC-X 还支持一些语言扩展：

1. 自定义中缀运算符。例如：
    ```c
    int f(int x, int y) { return (x - 1) * (y - 1); }
    int main()
    {
        int x = 2, y = 2;
        return x ~f~ y; // 等价于 return f(x, y);
    }
    ```
2. 各类复合赋值运算符，如 `+=`，`*=`。
3. 复合运算符返回左值，这意味着可以写 `x = y = z;` 这种代码。
4. 自增自减运算符，其中前缀自增自减返回左值。例如 `++x = 1;`
5. 以常量下标取得的常量数组元素可以参与编译期常量表达式求值，例如：
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
    - Source Han Serif SC VF
    - Source Han Sans SC VF
    - STKaiti（华文楷体）
    - Noto Serif（可变字体版本）与 Noto Serif Italic（可变字体版本）
    - SourceCodeVF 与 SourceCodeVF Italic
    - SourceSans3VF 与 SourceSans3VF Italic
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
