설치
====

이 문서에서는 Dojang을 설치하는 방법들을 설명합니다.


Homebrew (macOS 및 Linux)
-------------------------

Dojang은 [Homebrew]를 통해 자동으로 빌드하여 설치할 수 있습니다.
다음 명령어를 터미널에서 입력해 주세요.

~~~~ console
$ brew tap dahlia/dojang https://github.com/dahlia/dojang.git
$ brew install dahlia/dojang/dojang
~~~~

[Homebrew]: https://brew.sh/


Scoop (Windows)
---------------

Dojang은 [Scoop]을 통해 자동으로 실행 파일을 다운로드 받아 설치할 수 있습니다.
다음 명령어를 터미널에서 입력해 주세요.

~~~~ console
$ scoop bucket add dojang https://github.com/dahlia/dojang.git
$ scoop install dojang
~~~~

[Scoop]: https://scoop.sh/


직접 빌드하기
-------------

Dojang은 Haskell로 만들어진 프로그램입니다.  따라서 [Haskell Tool Stack]을
설치해야 합니다.  Stack 공식 홈페이지의 [설치 설명서][1]를 참고하여
Stack을 설치해 주세요.  터미널에서 `stack` 명령어를 사용할 수 있으면
설치가 완료된 것입니다.

이제 Dojang을 빌드할 준비가 되었습니다.  터미널에서 다음 명령어를 입력해 주세요.

~~~~ console
$ git clone https://github.com/dahlia/dojang.git
$ cd dojang/
$ stack build
$ stack install
~~~~

`stack install` 명령어는 *~/.local/bin* 디렉터리에 `dojang` 실행 파일을
설치합니다.  이제 `dojang` 명령어를 사용할 수 있습니다.

만약 다른 디렉터리에 설치하고 싶다면 `stack install` 명령어를 `--local-bin-path`
옵션과 함께 사용해 주세요.  예를 들어, 다음 명령어는 *~/bin* 디렉터리에 `dojang`
실행 파일을 설치합니다.

~~~~ console
$ stack install --local-bin-path ~/bin
~~~~

[Haskell Tool Stack]: https://haskellstack.org/
[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
