설치
====

이 문서에서는 Dojang을 설치하는 방법들을 설명합니다.


검증 설치 스크립트
------------------

릴리스 설치 스크립트는 실행 파일과 해당 릴리스의 *SHA256SUMS* 파일을
다운로드합니다.  선택한 아카이브의 체크섬을 확인한 뒤 설치합니다.

Linux 또는 macOS에서는 다음 명령을 실행합니다.

~~~~ bash
curl -fsSLO https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.sh
less install.sh
sh ./install.sh
~~~~

POSIX 설치 스크립트는 x86-64와 AArch64를 지원하며, 기본 설치 경로는
*~/.local/bin*입니다.

x86-64 Windows에서는 PowerShell에서 다음 명령을 실행합니다.

~~~~ powershell
Invoke-WebRequest `
  https://raw.githubusercontent.com/dahlia/dojang/main/scripts/install.ps1 `
  -OutFile install.ps1 `
  -UseBasicParsing
Get-Content .\install.ps1
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force
.\install.ps1
~~~~

다른 디렉터리를 선택하려면 `DOJANG_INSTALL_DIR`을 설정하세요.  최신 버전 대신
특정 릴리스를 설치하려면 스크립트를 실행하기 전에
`DOJANG_INSTALL_VERSION`을 `0.3.0` 같은 값으로 설정합니다.

선택한 아카이브를 설치하거나 압축을 풀지 않고 다운로드하여 검증하려면
`DOJANG_INSTALL_DOWNLOAD_DIR`을 명시적인 디렉터리로 설정하세요.  이
디렉터리에는 아카이브와 릴리스의 *SHA256SUMS* 파일이 함께 저장됩니다.  위와
같이 `install.sh`를 다운로드하여 확인한 다음 실행하세요.

~~~~ bash
DOJANG_INSTALL_DOWNLOAD_DIR="$PWD/dojang-download" sh ./install.sh
~~~~

Windows에서는 위와 같이 `install.ps1`을 다운로드하여 확인한 다음 같은 환경
변수를 설정하고 로컬 스크립트를 실행합니다.

~~~~ powershell
$env:DOJANG_INSTALL_DOWNLOAD_DIR = "$PWD\dojang-download"
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass -Force
.\install.ps1
~~~~


Homebrew (macOS 및 Linux)
-------------------------

Dojang은 [Homebrew]를 통해 자동으로 실행 파일을 다운로드 받아 설치할 수
있습니다.  다음 명령을 터미널에서 입력해 주세요.

~~~~ bash
brew tap dahlia/dojang https://github.com/dahlia/dojang.git
brew install --cask dahlia/dojang/dojang
~~~~

> [!TIP]
>
> 만약 실행 파일을 받지 않고 직접 빌드하고 싶다면 `--cask` 옵션 대신 `--formula`
> 옵션을 사용해 주세요.

[Homebrew]: https://brew.sh/


Scoop (Windows)
---------------

Dojang은 [Scoop]을 통해 자동으로 실행 파일을 다운로드 받아 설치할 수 있습니다.
다음 명령을 터미널에서 입력해 주세요.

~~~~ bash
scoop bucket add dojang https://github.com/dahlia/dojang.git
scoop install dojang
~~~~

[Scoop]: https://scoop.sh/


mise (크로스플랫폼)
-------------------

개발 도구 관리에 [mise]를 사용하고 있다면, GitHub 백엔드를 통해 Dojang을 설치할
수 있습니다.  다음 명령을 터미널에서 입력해 주세요.

~~~~ bash
mise use -g github:dahlia/dojang
~~~~

[mise]: https://mise.jdx.dev/


컨테이너 이미지와 정적 Linux 실행 파일
--------------------------------------

Linux 릴리스는 x86-64 및 AArch64용 멀티 아키텍처 컨테이너 이미지도
게시합니다.  태그가 붙은 이미지는 다음처럼 확인할 수 있습니다.

~~~~ bash
docker run --rm ghcr.io/dahlia/dojang:0.3.0 version
~~~~

이미지에는 정적으로 링크된 실행 파일이 들어 있습니다.  이미지와 같은 프로세서
아키텍처를 사용하는 Linux 머신에서는 다음처럼 꺼낼 수 있습니다.

~~~~ bash
docker run --rm --entrypoint cat ghcr.io/dahlia/dojang:0.3.0 \
  /usr/local/bin/dojang > dojang
chmod +x dojang
~~~~


직접 빌드하기
-------------

Dojang은 Haskell로 만들어진 프로그램입니다.  따라서 [Haskell Tool Stack]을
설치해야 합니다.  Stack 공식 홈페이지의 [설치 설명서][1]를 참고하여
Stack을 설치해 주세요.  터미널에서 `stack` 명령을 사용할 수 있으면
설치가 완료된 것입니다.

이제 Dojang을 빌드할 준비가 되었습니다.  터미널에서 다음 명령을 입력해 주세요.

~~~~ bash
git clone https://github.com/dahlia/dojang.git
cd dojang/
stack build
stack install
~~~~

`stack install` 명령의 설치 경로는 플랫폼과 Stack 설정에 따라 달라질 수
있습니다.  다음 명령으로 실제 설치 경로를 확인해 주세요.

~~~~ bash
stack path --local-bin
~~~~

필요하다면 이 디렉터리를 `PATH`에 추가한 뒤 `dojang` 명령을 사용해 주세요.

만약 다른 디렉터리에 설치하고 싶다면 `stack install` 명령을 `--local-bin-path`
옵션과 함께 사용해 주세요.  예를 들어, 다음 명령은 *~/bin* 디렉터리에 `dojang`
실행 파일을 설치합니다.

~~~~ bash
stack install --local-bin-path ~/bin
~~~~

[Haskell Tool Stack]: https://haskellstack.org/
[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
