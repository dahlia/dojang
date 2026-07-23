저장소 부트스트랩
=================

`dojang init --from SOURCE`는 기존 저장소를 새 머신으로 가져오고, 해당 머신을
등록한 다음, 최초 `dojang apply`를 실행할지 묻습니다.  가져오기 작업은 대상
디렉터리와 같은 상위 디렉터리에 만든 임시 디렉터리에서 진행됩니다.  Dojang은
준비된 *dojang.toml*을 확인한 뒤 저장소를 게시하므로, 올바르지 않거나 불완전한
원본이 대상 디렉터리를 대체하지 않습니다.

대상 디렉터리는 존재하지 않거나 비어 있어야 합니다.  `-r` 또는
`--repository-dir`로 다른 경로를 선택하지 않으면 현재 디렉터리를 사용합니다.


로컬 디렉터리와 아카이브
------------------------

로컬 디렉터리에는 전송 방식 옵션이 필요하지 않습니다.

~~~~ console
$ dojang -r ~/.dotfiles init --from /media/backup/dotfiles
~~~~

Dojang은 심볼릭 링크를 따라가지 않고 링크로 복사합니다.  `.zip`, `.tar`,
`.tar.gz`, `.tgz` 아카이브도 사용할 수 있습니다.

~~~~ console
$ dojang -r ~/.dotfiles init --from ~/Downloads/dotfiles.tar.gz
~~~~

아카이브 항목은 압축을 풀기 전에 검사합니다.  절대 경로, 상위 디렉터리 순회,
역슬래시 경로, 링크, 서로 충돌하는 항목은 거부합니다.


외부 전송 방식
--------------

Git, HTTPS 클라이언트 같은 네트워크 도구는 머신별 로컬 외부 전송 방식으로
설정합니다.  Dojang은 셸을 거치지 않고 설정한 실행 파일을 직접 시작합니다.
원본과 임시 목적지는 각각 하나의 완전한 인자로 전달되므로, 공백과 셸 특수
문자는 명령 문법이 아니라 데이터로 유지됩니다.

기본 설정 파일 경로는 다음과 같습니다.

 -  Linux 및 그 밖의 POSIX 시스템:
    *$XDG\_CONFIG\_HOME/dojang/transports.toml*. `XDG_CONFIG_HOME`이 없거나 상대
    경로이면 *~/.config/dojang/transports.toml*
 -  macOS: *~/Library/Application Support/dojang/transports.toml*
 -  Windows: *%APPDATA%\\dojang\\transports.toml*. `APPDATA`가 없으면
    *%USERPROFILE%\\AppData\\Roaming\\dojang\\transports.toml*

다음 예시는 Git 전송 방식을 추가합니다.

~~~~ toml
[transports.git]
command = ["git", "clone", "--", "{source}", "{destination}"]
inherit-environment = ["HOME", "PATH", "SSH_AUTH_SOCK"]

[transports.git.environment]
GIT_TERMINAL_PROMPT = "1"
~~~~

각 명령에는 완전한 인자 하나를 차지하는 `{source}`와 `{destination}` 자리
표시자가 정확히 하나씩 있어야 합니다.  실행 파일에는 자리 표시자를 쓸 수
없습니다.  외부 전송 방식은 `inherit-environment`에 이름을 적지 않은 호스트
환경 변수를 받지 않습니다.  `environment`의 값은 물려받은 값을 덮어씁니다.
이 파일에는 인증 정보를 저장하지 마세요.

전송 방식 이름을 지정하여 사용합니다.

~~~~ console
$ dojang -r ~/.dotfiles init \
>   --from git@github.com:USER/dotfiles.git \
>   --transport git
~~~~

다른 설정 파일을 선택하려면 `--transport NAME`과 함께
`--transport-file PATH`를 사용합니다.  전송 방식 이름은 대소문자를 구분하고
ASCII 문자로 시작해야 하며, ASCII 문자, 숫자, 하이픈, 밑줄을 쓸 수 있습니다.
`directory`와 `archive`는 예약된 이름입니다.


머신 등록과 최초 적용
---------------------

가져온 뒤에는 `dojang init`이 선언 파일을 확인하고 현재 머신을 등록합니다.
일반 저장소 초기화와 같은 `--fact KEY=VALUE` 및 `--facts-file PATH` 옵션을
사용할 수 있습니다.  이어서 저장소를 적용할지 묻습니다.  적용하지 않으면 대상
파일을 바꾸지 않고, 올바르게 등록된 체크아웃만 남습니다.

비대화식 부트스트랩에는 명시적인 승인이 필요합니다.

~~~~ console
$ dojang -r ~/.dotfiles init \
>   --from /media/backup/dotfiles.tar.gz \
>   --no-interactive \
>   --yes
~~~~

`--yes`는 최초 변경 적용만 승인합니다.  선언 파일, 머신 정보, 대상 디렉터리,
전송 방식, 아카이브 검사를 건너뛰지 않습니다.


부트스트랩 미리 보기
--------------------

전역 `--dry-run` 옵션은 `init` 앞에 둡니다.

~~~~ console
$ dojang --dry-run -r ~/.dotfiles init --from /media/backup/dotfiles
~~~~

모의 실행은 저장소를 게시하거나 머신 등록을 저장하거나 파일을 적용하지
않습니다.  외부 전송 방식에서는 값이 가려진 실행 요청만 출력하고 프로그램을
시작하지 않습니다.  따라서 파일을 가져오지 않으며 원격 선언 파일도 확인할 수
없습니다.  실제 부트스트랩을 실행하면 임시 디렉터리에서 선언 파일을 확인합니다.
