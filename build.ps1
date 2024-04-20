# Copyright (C) 2024 Elkeid-me
#
# This file is part of Xenon ATC-X.
#
# Xenon ATC-X is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Xenon ATC-X is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Xenon ATC-X.  If not, see <http://www.gnu.org/licenses/>.

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