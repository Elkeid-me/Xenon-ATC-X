function Build-Xenon { cargo build -r }

function Build-Doc {
    Set-Location doc
    lualatex -shell-escape doc.tex
    Set-Location ..
}

function Build-Doc-Default-Fonts {
    Set-Location doc
    lualatex -shell-escape "\NewDocumentCommand{\DefaultFonts}{}{}\input{doc.tex}"
    Set-Location ..
}

if ($args.Count -eq 0) { Build-Xenon }
elseif ($args.Count -eq 1) {
    Switch ($args[0]) {
        "xenon" { Build-Xenon }
        "doc" { Build-Doc }
        "doc-default-fonts" { Build-Doc-Default-Fonts }
        "test-koopa" { docker run -it --rm -v ${PWD}\:/root/compiler maxxing/compiler-dev autotest -koopa /root/compiler/ }
        "test-risc-v" { docker run -it --rm -v ${PWD}\:/root/compiler maxxing/compiler-dev autotest -riscv /root/compiler/ }
        "all" {
            $build = Start-Job -Name "Build Xenon" -ScriptBlock ${Function:Build-Xenon}
            $doc = Start-Job -Name "Build Doc" -ScriptBlock ${Function:Build-Doc}
            Wait-Job -Job $build, $doc
        }
    }
}