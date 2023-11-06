선언
====

*dojang.toml*이라는 파일 이름의 **선언**(manifest) 파일은 해당 파일이 있는
디렉터리가 Dojang이 관리하는 설정 파일 저장소임을 선언하며,
그 설정 파일들이 실제 컴퓨터에 어떻게 적용되어야 할지를 설정합니다.

확장자에서 미루어 짐작할 수 있듯 [TOML] 형식으로 되어 있으며,
크게 `dirs`, `files`, `monikers`, `ignores` 네 구획으로 나뉩니다.
본 문서에서는 독자가 TOML의 기본적인 문법을 안다는 전제로 설명합니다.

[TOML]: https://toml.io/


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
문법을 쓸 수 있습니다.  실제로, 위 예시는 아래와 같이 환경 서술어 문법으로
고쳐서 써도 동작이 같습니다.

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


디렉터리와 파일의 경로 라우팅
-----------------------------

[**라우팅**](routing.ko.md)이란 설정 파일을 환경에 따라 다른 경로에 적용하기
위한 것으로, 예를 들어 홈 디렉터리는 Linux, macOS, Windows 세 플랫폼에서 모두
다르므로 다음과 같이 라우팅할 수 있습니다.

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

라우팅에는 위 예시의 `home_directory`와 같이 이름을 정할 수 있는데,
이 이름은 실제로 설정 파일의 소스가 저장소 안에서 위치한 경로를 나타냅니다.
`home_directory`라고 적혀 있다면 선언 파일이 있는 디렉터리 안의
*home_directory/*를 가리키게 됩니다. 저장소에 아래와 같이 파일이 있다고 한다면…

- *dojang.manifest*
- *home_directory/*
    - *.bash_profile*
    - *.inputrc*

Linux에 적용할 경우 아래 경로에 파일들이 복사됩니다.

- */home/$USER/.bash_profile*
- */home/$USER/.inputrc*

마찬가지로, Windows에서는 아래와 같습니다.

- *C:\Users\\%USERNAME%\\.bash_profile*
- *C:\USers\\%USERNAME%\\.inputrc*

실제로는 더 복잡한 라우팅이 가능한데, 라우팅에 관한 자세한 설명은 [해당
문서](routing.ko.md)를 참고하세요.


무시할 파일 목록
----------------

대상 경로에는 상관 없는 파일이 잔뜩 있기 마련입니다.  이를 냅둔 채
`dojang status` 명령을 쓰면 상관 없는 변경들로 화면이 꽉 차서 실제로 쓰기가
어려울 것입니다.  이를 피하기 위해 대상 경로에서 무시할 파일 목록을 나열할
수 있습니다.

무시할 파일 목록은 `ignores` 구획 안에 소스 디렉터리, 즉 `dirs` 구획에서 설정한
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


<!-- cSpell:ignore inputrc -->
