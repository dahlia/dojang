머신 정보
=========

머신 정보(machine facts)는 운영체제, 아키텍처, 커널이 같은 머신도 저장소마다
구분할 수 있게 해 주는 이름 붙은 문자열 값입니다.  키와 값은 대소문자를
구분하지 않습니다.  예를 들어 업무용 머신에서만 파일을 적용할 수 있습니다:

~~~~ toml
[monikers.work]
when = 'fact.class = "work"'
~~~~

머신 정보는 저장소별 설정이지 비밀 저장소가 아닙니다.  이름과 값이 진단 메시지,
머신 상태, 경로 규칙 출처, 훅 지문에 나타날 수 있습니다.  비밀번호, 토큰 같은
자격 증명은 저장하지 마세요.


키와 값
-------

머신 정보 키는 점으로 구분한 하나 이상의 구획으로 이루어집니다.  각 구획은
ASCII 문자로 시작해야 하고, 그 뒤에는 ASCII 문자, 숫자, 붙임표, 밑줄을 쓸 수
있습니다.  `class`, `org.team`, `hardware.gpu-model`은 올바른 키입니다.  키와
값은 대소문자를 구분하지 않지만, 파일에 쓸 때는 원래 표기를 유지합니다.

Dojang은 다음 내장 머신 정보를 감지합니다:

 -  `os`: 운영체제
 -  `arch`: 프로세서 아키텍처
 -  `kernel`: 커널 이름
 -  `kernel-release`: 커널 릴리스
 -  `hostname`: 네트워크 호스트 이름

기존 `os`, `arch`, `kernel`, `kernel-release` 서술어는 그대로 쓸 수 있습니다.
`fact.os = linux` 같은 일반형도 같은 방식으로 일치하고 구체성도 같습니다.  내장
값은 `[facts]` 표나 `--fact`로 저장할 수 없습니다.  테스트에서 내장 값을 바꿀
때는 환경 시뮬레이션을 사용하세요.


머신 정보 선언하기
------------------

재사용할 머신 정보는 TOML 파일의 `[facts]` 표에 둡니다:

~~~~ toml
[facts]
class = "work"
"org.team" = "platform"
~~~~

기존 저장소에서 `dojang init`을 실행하면 현재 머신을 등록합니다.  이 명령은 경로
규칙과 훅의 서술어를 검사하고 참조한 모니커를 따라가며, 값이 없는 사용자 정의
머신 정보를 묻습니다.  서술어 정규화에서 제거되는 분기의 정보는 묻지 않습니다.

스크립트, CI, Windows 등 비대화형 환경에서는 값을 명시하세요:

~~~~ console
$ dojang init --no-interactive --fact class=work
$ dojang init --no-interactive --facts-file ~/.config/dojang/work.toml
~~~~

`--fact`는 저장소별 값을 머신 상태에 저장합니다.  옵션을 여러 번 쓸 수 있고 같은
키는 마지막 값이 우선합니다.  `--facts-file`은 값 대신 파일을 저장소와
연결하므로 여러 저장소가 값을 복사하지 않고 같은 프로필을 쓸 수 있습니다.
체크아웃 안의 파일은 상대 경로로 저장되어 체크아웃을 옮겨도 따라갑니다.
체크아웃 밖의 파일은 절대 경로로 저장되며 그 위치에 남아 있어야 합니다.

선언 파일 옆의 *dojang-env.toml*에 비어 있지 않은 `[facts]` 표가 있으면
`dojang init`이 자동으로 연결합니다.  공유 프로필에는 `--facts-file`을 명시하는
편이 더 분명합니다.  `--dry-run`은 등록 내용을 검사하고 보고할 수 있지만 답이나
파일 연결을 저장하지 않습니다.

비대화형 등록에서 필요한 값이 빠지면 종료 코드 22와 함께 모든 키를 보여 줍니다.
Windows에서는 대화형 질문도 지원하지 않으므로 `--fact`나 `--facts-file`을
전달하세요.  `dojang forget`은 다른 머신별 로컬 상태와 함께 이 저장소의 값과
파일 연결을 제거합니다.


우선순위와 시뮬레이션
---------------------

Dojang은 다음 순서로 환경을 구성하며, 뒤의 출처가 사용자 정의 키에 우선합니다:

1.  호스트 이름을 포함하여 감지한 내장 값
2.  연결한 머신 정보 파일
3.  `--fact`나 대화형 질문으로 저장한 저장소별 값
4.  `-e`/`--env-file`로 선택한 환경 시뮬레이션

마지막 출처는 테스트용 재정의입니다.  전체 환경 파일에는 계속해서 `os`, `arch`,
`[kernel]`이 있어야 하며, 최상위 `hostname`과 `[facts]` 표도 넣을 수 있습니다:

~~~~ toml
os = "linux"
arch = "aarch64"
hostname = "test-host"

[kernel]
name = "Linux"
release = "6.12.0"

[facts]
class = "test"
~~~~

감지한 호스트 정보만 확인하려면 `dojang env --ignore-env-file`을 사용하세요.
호스트 이름은 훅 프로세스에 `DOJANG_HOSTNAME`으로 전달됩니다.  사용자 정의 머신
정보는 `on-change` 지문에 포함되지만 개별 환경 변수로 내보내지는 않습니다.


서술어에서 머신 정보 사용하기
-----------------------------

표준 표기는 `fact.` 뒤에 머신 정보 키를 붙이는 방식입니다:

~~~~~ text
fact.class = work
fact.org.team != personal
fact.hardware.gpu in (integrated, discrete)
fact.location not in (home, travel)
~~~~ text

일반 머신 정보에는 `=`, `!=`, `in`, `not in`을 쓸 수 있습니다.  접두·접미 비교는
계속 `kernel-release`에만 쓸 수 있습니다.  정의되지 않은 머신 정보를 참조한
서술어는 거짓으로 평가되고 경고가 표시됩니다.

`&&`, `||`, `!`, 모니커와 조합하는 방법은 [환경 서술어]를 참고하세요.

[환경 서술어]: environment-predicate.ko.md
~~~~~
