선언
====

*dojang.toml*이라는 파일 이름의 **선언**(manifest) 파일은 해당 파일이 있는
디렉터리가 Dojang이 관리하는 설정 파일 저장소임을 선언하며,
그 설정 파일들이 실제 기기에 어떻게 적용되어야 할지를 설정합니다.

확장자에서 미루어 짐작할 수 있듯 [TOML] 형식으로 되어 있으며,
크게 `vars`, `dirs`, `files`, `monikers`, `ignores`, `hooks` 여섯 구획으로
나뉩니다.
본 문서에서는 독자가 TOML의 기본적인 문법을 안다는 전제로 설명합니다.

[TOML]: https://toml.io/


저장소 식별자
-------------

최상위 `repository-id` 필드는 `dojang init` 또는 `dojang migrate`가 만드는
UUID입니다.

~~~~ toml
repository-id = "123e4567-e89b-42d3-a456-426614174000"
~~~~

이 식별자는 저장소와 함께 이동하며 체크아웃을 옮기거나 이름을 바꿔도 바뀌지
않습니다.  직접 고치거나 다른 저장소에 복사하지 마세요.  Dojang은 이 값을
사용해 각 기기의 저장소별 [머신 상태]를 분리합니다.

[머신 상태]: machine-state.ko.md


모니커
------

**모니커**(moniker)는 [**환경**](environment.ko.md)에 대한 조건들에 붙는
이름입니다.  모니커는 쓰는 사람에 따라 자유롭되 알아보기 쉽게 정하면 됩니다.
이를테면 아래는 가장 흔하면서도 단순한 모니커 정의입니다.

~~~~ toml
[monikers.windows]
os = "windows"

[monikers.posix]
os = ["linux", "macos"]
~~~~

이 모니커를 통해 설정 파일들을 환경에 따라 조건적으로 적용할 수 있습니다.
예를 들어, 구형 Intel MacBook Pro와 신형 Apple silicon MacBook Pro를 함께 쓰는
사용자가 있다면, 아래와 같이 모니커들을 구성할 수 있을 것입니다.

~~~~ toml
[monikers.mac]
os = "macos"

[monikers.apple_silicon_mac]
any = ["mac"]
arch = "aarch64"

[monikers.intel_mac]
any = ["mac"]
arch = "x86_64"
~~~~

위 예시에 쓰인 필드 중 `any`는 다른 모니커 조건을 가리키는 데에 쓰입니다.
목록 안에 있는 모니커 중에서 하나라도 만족하면 만족되며, 나열된 모든 모니커를
만족해야 만족하는 조건을 원한다면 `all` 필드를 쓸 수 있습니다.

환경에 대한 조건이 복잡하다면 [**환경 서술어**](environment-predicate.ko.md)
문법을 `when` 필드에서 쓸 수 있습니다.  실제로, 위 예시는 아래와 같이 환경
서술어 문법으로 고쳐서 써도 동작이 같습니다.

~~~~ toml
[monikers.mac]
when = "os = macos"

[monikers.apple_silicon_mac]
when = "moniker = mac && arch = aarch64"

[monikers.intel_mac]
when = "moniker = mac && arch = x86_64"
~~~~

환경 서술어 문법에 대한 자세한 설명은 [해당 문서](environment-predicate.ko.md)를
참고하세요.

### 모니커를 정의할 때 쓸 수 있는 필드 목록

 -  `os` (문자열 또는 문자열의 배열): 환경의 운영체제.  배열일 경우 배열 안의
    모든 운영체제 중 하나라도 만족하면 만족.  운영체제 식별자는 [인식하는
    운영체제 목록](environment.ko.md#인식하는-운영체제-목록) 참고.

 -  `arch` (문자열 또는 문자열의 배열): 환경의 프로세서 아키텍처.  배열일 경우
    배열 안의 모든 아키텍처 중 하나라도 만족하면 만족.  아키텍처 식별자는
    [인식하는 프로세서 아키텍처
    목록](environment.ko.md#인식하는-프로세서-아키텍처-목록) 참고.

 -  `kernel` (문자열 또는 문자열의 배열): 환경의 커널 이름.  배열일 경우 배열
    안의 모든 커널 이름 중 하나라도 만족하면 만족.  커널 이름의 인식에 관해서는
    [커널 인식](environment.ko.md#커널-인식) 참고.

 -  `kernel-release` (문자열 또는 문자열의 배열): 환경의 커널 버전.  배열일 경우
    배열 안의 모든 커널 버전 중 하나라도 만족하면 만족.  커널 버전의 인식에
    관해서는 [커널 인식](environment.ko.md#커널-인식) 참고.

 -  `any` (문자열의 배열): 다른 모니커 이름들.  배열 안의 모든 모니커 중
    하나라도 만족하면 만족.  반드시 하나 이상의 원소가 있어야 한다.

 -  `all` (문자열의 배열): 다른 모니커 이름들.  배열 안의 모든 모니커를
    만족해야 만족.  반드시 하나 이상의 원소가 있어야 한다.

 -  `when` (문자열): [환경 서술어](environment-predicate.ko.md) 문법으로
    작성된 조건.  조건을 만족하면 만족.  위의 필드들과 함께 쓸 경우 모든
    조건을 함께 만족해야 만족.


선언 변수
---------

선택 사항인 `vars` 구획에서는 [파일 경로 표현식]에서 재사용할 선언 변수를
정의합니다.  간결 선언은 조건 없이 항상 쓰는 값입니다.

~~~~ toml
[vars]
CONFIG_HOME = "${XDG_CONFIG_HOME:-$HOME/.config}"
CACHE_HOME = "$CONFIG_HOME/cache"
~~~~

이름은 대소문자를 구분하며 `[A-Za-z_][A-Za-z0-9_]*`와 일치해야 합니다.
상속받은 환경 변수와 마찬가지로 `$NAME` 또는 `${NAME}` 문법으로 참조합니다.
선언 파일의 테이블 순서와 관계없이 선언 변수끼리 서로 참조할 수 있습니다.
참조가 순환하면 Dojang은 파일을 적용하지 않고 순환 경로를 오류로 알립니다.

환경에 따라 다른 값을 쓰려면 순서가 있는 분기 테이블 배열을 사용합니다.

~~~~ toml
[vars]
TOOLS_HOME = [
  { when = "fact.class = work", value = "$HOME/work/tools" },
  { moniker = "windows", value = "$UserProfile/tools" },
  { when = "always", value = "$HOME/tools" },
]
~~~~

각 분기에는 `value`와 `moniker` 또는 `when` 중 정확히 하나가 있어야 합니다.
선언 순서대로 검사해 처음 일치한 분기를 사용합니다.  아무 분기도 일치하지 않으면
같은 이름으로 상속받은 환경 변수를 조회합니다.  선택된 빈 문자열은 이와 다르게
정의된 빈 값이며, 상속받은 변수를 가립니다.

선언 변수는 상속받은 환경 변수보다 우선합니다.  머신 정보와 다른 환경 서술어는
분기를 선택할 뿐 변수 값으로 자동 노출되지 않습니다.  `dojang init`은 도달
가능한 선언 변수 분기에서 참조하는 사용자 정의 머신 정보를 묻습니다.  선언 변수
값은 비밀 정보가 아닌 설정으로 다뤄야 합니다.  Dojang은 경로 규칙 레코드와 훅
지문에 평문 값을 저장하지 않지만, 프로세스와 진단 정보에서는 확장된 경로를 볼
수 있습니다.

[파일 경로 표현식]: file-path-expression.ko.md


디렉터리와 파일의 경로 지정
---------------------------

[**경로 지정**](routing.ko.md)이란 설정 파일을 환경에 따라 다른 경로에 적용하기
위한 것으로, 예를 들어 홈 디렉터리는 Linux, macOS, Windows 세 플랫폼에서 모두
다르므로 다음과 같이 경로를 지정할 수 있습니다.

~~~~ toml
[dirs.home_directory]
linux = "/home/$USER"
mac = "/Users/$USER"
win = "C:\\Users\\$UserName"

[monikers.linux]
os = "linux"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

`$USER`나 `$UserName`과 같은 문법은 해당 환경 변수의 값으로 치환되며,
이에 관한 더 자세한 설명은 [파일 경로 표현식](file-path-expression.ko.md)
문서에 나와 있습니다.

물론, 위 예시는 설명을 위해 작위적으로 만든 것이며,
실제로는 아래처럼 더 간단하면서 정확하게 할 것입니다:

~~~~ toml
[dirs.home_directory]
posix = "$HOME"
windows = "$UserProfile"

[monikers.posix]
os = ["linux", "macos"]

[monikers.windows]
os = ["windows"]
~~~~

이 간결 경로 규칙 문법은 각 모니커를 경로에 바로 대응시킵니다.  모든 분기가 서로
다른 모니커를 쓸 때 가장 짧게 표현할 수 있습니다.  환경 서술어를 직접 쓰거나,
같은 조건을 반복하거나, 두 형식을 한 경로 규칙에서 함께 쓰려면 경로 규칙을
순서가 있는 분기 테이블 배열로 작성합니다.

~~~~ toml
[[files.".bashrc"]]
moniker = "posix"
path = "$HOME/.bashrc"

[[files.".bashrc"]]
when = "os = windows && arch = aarch64"
path = "$UserProfile/.bashrc"

[[files.".bashrc"]]
when = "os = android"
~~~~

상세 경로 규칙의 각 분기에는 `moniker`와 `when` 중 정확히 하나만 있어야 합니다.
`moniker` 값은 같은 선언 파일에 정의된 모니커 이름이어야 하며, `when`에는
[환경 서술어](environment-predicate.ko.md)를 쓸 수 있습니다.  `path` 필드는
선택 사항입니다.  이를 생략하면 해당 조건에 대한 공 경로 규칙이 됩니다.
Dojang은 `path = ""`로 쓰면 이와 달리 명시적인 빈 경로 표현식을 나타냅니다.
Dojang은 반복된 조건을 포함해 상세 분기의 경로 규칙 순서를 보존합니다.  분기는
그 순서대로 검사하며, 처음 일치한 분기가 대상 경로를 결정합니다.  간결 경로
규칙에서는 기존과 같이 서술어의 구체성으로 우선순위를 결정합니다.

경로 규칙에는 위 예시의 `home_directory`와 같이 이름을 정할 수 있는데,
이 이름은 실제로 설정 파일의 원본이 저장소 안에서 위치한 경로를 나타냅니다.
`home_directory`라고 적혀 있다면 선언 파일이 있는 디렉터리 안의
\*home\_directory/\*를 가리키게 됩니다. 저장소에 아래와 같이 파일이 있다고
한다면…

 -  *dojang.manifest*
 -  *home\_directory/*
     -  *.bash\_profile*
     -  *.inputrc*

Linux에 적용할 경우 아래 경로에 파일들이 복사됩니다.

 -  */home/$USER/.bash\_profile*
 -  */home/$USER/.inputrc*

마찬가지로, Windows에서는 아래와 같습니다.

 -  *C:\\Users\\%USERNAME%\\.bash\_profile*
 -  *C:\\USers\\%USERNAME%\\.inputrc*

실제로는 더 복잡한 경로 지정이 가능한데, 자세한 설명은 [해당
문서](routing.ko.md)를 참고하세요.


무시할 파일 목록
----------------

대상 경로에는 상관 없는 파일이 잔뜩 있기 마련입니다.  이를 냅둔 채
`dojang status` 명령을 쓰면 상관 없는 변경들로 화면이 꽉 차서 실제로 쓰기가
어려울 것입니다.  이를 피하기 위해 대상 경로에서 무시할 파일 목록을 나열할
수 있습니다.

무시할 파일 목록은 `ignores` 구획 안에 원본 디렉터리, 즉 `dirs` 구획에서 설정한
디렉터리 이름과 짝지어 설정합니다.  `files` 구획에서 설정한 파일들은 단일 파일
단위기 때문에 무시할 수 없습니다.  목록 내 각 항목은 글랍(glob)과 유사한 패턴
문법으로 기술합니다.

~~~~ toml
[dirs.HOME]
posix = "$HOME"
windows = "$UserProfile"

[ignores]
HOME = [
  ".config",
  ".DS_Store",
]
~~~~

아무리 대상 경로에서 무시된 파일이라 하더라도, 저장소에 포함된 파일이면
그 파일은 무시되지 않습니다.  이를 이용해서 모든 파일(`*`)을 무시하되 저장소에
추가된 파일에 한해서만 관리하는 것도 가능합니다.


훅
--

\*\*[훅](hooks.ko.md)\*\*은 `apply`, `reflect`, `diff`, `status`, `edit`,
`unmanage` 전후에 사용자 정의 스크립트를 실행합니다.  각 명령에는 `pre-*`와
성공 시 `post-*` 이벤트가 있습니다.  `apply`의 최초 적용 이벤트도 그대로
지원합니다.  훅은 `always`, 저장소와 머신마다 한 번 실행하는 `once`, 명시적인
개정 키가 바뀌면 실행하는 `on-change` 정책을 사용할 수 있습니다.

~~~~ toml
[[hooks.pre-apply]]
command = "/bin/echo"
args = ["설정 파일 적용 중..."]

[[hooks.post-apply]]
id = "reload-service"
policy = "on-change"
change-key = "service-config-v2"
command = "/usr/bin/systemctl"
args = ["--user", "restart", "my-service"]
when = "os = linux"
~~~~

훅에 대한 자세한 설명은 [훅 문서](hooks.ko.md)를 참고하세요.

<!-- cSpell:ignore inputrc -->
