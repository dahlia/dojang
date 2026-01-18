라우팅
======

같은 설정 파일이라도 환경에 따라 놓여야 하는 위치가 다른 경우는 흔합니다.
예를 들어 PostgreSQL의 클라이언트인 `psql`의 설정 파일은 [문서][1]에 따르면
환경에 따라 다음과 같이 경로가 달라집니다.

 -  POSIX: `PSQLRC` 환경 변수가 있다면 해당 값을 경로로 사용하되,
    없다면 *~/.psqlrc* 사용.
 -  Windows: `PSQLRC` 환경 변수가 있다면 해당 값을 경로로 사용하되,
    없다면 *%APPDATA%\\postgresql\\psqlrc.conf* 사용.

Dojang은 이러한 복잡한 경우에 대응하기 위해 **라우팅**(routing)이라는 개념을
이용합니다.

[1]: https://www.postgresql.org/docs/current/app-psql.html#APP-PSQL-FILES-PSQLRC


디렉터리 라우팅
---------------

많은 프로그램이 설정 파일을 Linux에서는 `$XDG_CONFIG_HOME`에, macOS에서는
*~/Library/Application Support*에, Windows에서는 `%AppData%` 또는
`%LocalAppData%`에 저장합니다.[^1]  따라서 여러 환경에서 같은 설정 파일을
쓰려면 해당 경로들을 하나로 묶어서 라우팅하는 게 편합니다.
아래는 선언은 그 예를 보여줍니다.

~~~~ toml
[dirs.app_config]
linux = "$XDG_CONFIG_HOME"
mac = "$HOME/Library/Application Support"
win = "$AppData"

[monikers.linux]
os = "linux"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

여기서 라우팅 이름인 `app_config`는 저장소 내의 디렉터리 이름이기도 합니다.
저장소 안에 파일들이 아래와 같이 있다고 할 때 …

 -  *dojang.toml*
 -  *app\_config/*
     -  *alacritty/*
         -  *alacritty.toml*
     -  *ghc/*
         -  *ghci.conf*
     -  *nvim/*
        *init.vim*

Linux에서는 아래 경로에 파일들이 생길 것입니다.

 -  */home/$USER/.config/alacritty/*
 -  */home/$USER/.config/alacritty/alacritty.toml*
 -  */home/$USER/.config/ghc/*
 -  */home/$USER/.config/ghc/ghci.conf*
 -  */home/$USER/.config/nvim/*
 -  */home/$USER/.config/nvim/init.vim*

하지만 만약 `XDG_CONFIG_HOME` 환경 변수가 정의되어 있지 않다면 어떻게 될까요?
Linux에서의 `app_config` 라우팅은 빈 경로, 즉 Dojang을 실행시키는 시점의 현재
작업 디렉터리(CWD, current working directory)가 되어버릴 것입니다.
이를 대비하고 싶다면 아래와 같이 라우팅을 살짝 고쳐볼 수 있습니다.

~~~~ toml
linux = "${XDG_CONFIG_HOME:-$HOME/.config}"
~~~~

위 문법은 `XDG_CONFIG_HOME` 환경 변수가 있다면 그 값을 취하되,
그렇지 않다면 *~/.config* 경로를 따르라는 뜻입니다.
이와 같은 고급 기능에 대해서는 [파일 경로 표현식](file-path-expression.kr.md)
문서에 나와 있습니다.

[^1]: 예를 들어, 크로스플랫폼 앱 프레임워크 Electron에서는
      `app.getPath("appData")` API를 쓰면 환경에 따라 다른 경로를 반환합니다.
      마찬가지로, GLib의 `get_user_config_dir()` API도 비슷하게 동작합니다.

### 무관한 파일 무시

홈 디렉터리처럼 설정 파일 이외에도 다양한 자료가 있을 수 있는 경우,
설정 파일과 무관한 파일들이 너무 많이 보여 `dojang status` 결과를 맨눈으로
보기 어렵습니다.  게다가 파일이 너무 많을 경우 느려지기도 합니다.

이를 피하려면 `ignores` 구획에 무시할 파일들을 지정해 주면 됩니다.
예를 들어, 아래와 같이 `home` 라우팅을 추가했다고 합시다.

~~~~ toml
[dirs.home]
linux = "$HOME"
mac = "$HOME"
win = "$UserProfile"
~~~~

이제 `home` 라우팅이 라우팅하는 디렉터리 안에 있는 파일들은 모두
`dojang status` 결과에 나타납니다.  하지만 이 중에서도 *Documents* 디렉터리는
무시하고 싶을 수 있습니다.  이럴 때는 아래와 같이 `ignores` 구획에 무시할
파일들을 지정해 주면 됩니다.

~~~~ toml
[ignores]
home = [
  "Documents",
]
~~~~

실제로는 홈 디렉터리에는 무시할 파일이 그렇지 않은 파일보다 훨씬 많은 탓에,
그냥 모든 파일을 무시하는 게 대체로 더 편합니다.

~~~~ toml
[ignores]
home = ["*"]
~~~~

이렇게 모든 파일을 무시해도 `dojang reflect -f` 커맨드로 파일을 하나씩
지정하여 반영할 수 있습니다.  한 번 저장소에 추가된 파일은 무시 목록에
있어도 관리됩니다.

### 라우팅이 겹치는 경우

위 예시처럼 `app_config` 라우팅을 Linux에서 `XDG_CONFIG_HOME`,
즉 *~/.config*로 지정해 둔 상태에서, 홈 디렉터리에 대한 라우팅을 추가한다고
가정합시다.

~~~~ toml
[dirs.home]
linux = "$HOME"
mac = "$HOME"
win = "$UserProfile"
~~~~

대부분의 경우, `home`이 라우팅된 디렉터리 안에 `app_config`가 라우팅된
디렉터리도 포함하게 됩니다.  이렇게 두 개 이상의 라우팅에서 겹쳐지는 디렉터리가
있을 경우 관리할 때 실수하기 쉽기 때문에, 다음과 같이 한쪽에서 무시하게
해야 합니다.

~~~~ toml
[ignores]
home = [
  ".config",
  "AppData/Roaming",
  "Library/Application Support"
]
~~~~


단일 파일 라우팅
----------------

어떤 설정 파일의 경우 환경에 따라 디렉터리 뿐만 아니라 파일 이름 자체가
달라지기도 하는데, 그럴 때는 차라리 파일 단위로 라우팅하는 게 깔끔할 수
있습니다.  예를 들어, 본 문서의 맨 처음에 예시로 들었던 `psql` 설정 파일은
아래처럼 파일 단위로 라우팅될 수 있습니다.

~~~~ toml
[files.".psqlrc"]
posix = "${PSQLRC:-$HOME/.psqlrc}"
win = "${PSQLRC:-${AppData:-$UserProfile/AppData/Roaming}/postgresql/psqlrc.conf}"

[monikers.posix]
os = ["linux", "macos"]

[monikers.win]
os = "windows"
~~~~


라우팅 우선순위
---------------

단일 파일이나 디렉터리에 대해 현재 환경과 일치하는 라우팅이 여러 개 있을 경우,
Dojang은 **구체성**(specificity)을 기준으로 어떤 라우팅을 사용할지 결정합니다.
서술어가 구체적일수록 우선순위가 높습니다.  이를 통해 특수한 설정이 일반적인
설정보다 먼저 적용됩니다.

### 구체성 계산 방법

구체성은 서술어가 참이 되기 위해 충족해야 하는 조건의 수로 측정됩니다.
핵심 규칙은 다음과 같습니다.

 -  **단일 조건**(예: `os = linux`, `arch = "x86_64"`)의 구체성은 1입니다.

 -  **논리곱(`&&`)** 연산자는 각 부분의 구체성을 합산합니다.  예를 들어,
    `os = linux && arch = "x86_64"`의 구체성은 2입니다.
    두 조건 모두 충족되어야 하기 때문입니다.

 -  **논리합(`||`)** 연산자는 각 부분 중 최댓값을 취합니다.  예를 들어,
    `os = linux || os = macos`의 구체성은 1입니다.
    하나의 조건만 충족되면 되기 때문입니다.

 -  **모니커** 참조는 해석되는 서술어의 구체성을 물려받습니다.
    `os = linux && arch = "x86_64"`로 정의된 모니커는 해당 표현식을 직접 쓴 것과
    동일한 구체성을 갖습니다.

 -  \*\*`always`\*\*의 구체성은 0으로, 모든 환경과 일치하지만 가장 낮은 우선순위를
    갖습니다.

### 예시

다음과 같이 설정 파일에 대한 라우팅이 있는 선언을 생각해 봅시다.

~~~~ toml
[files.".bashrc"]
apple-silicon = "~/Library/bashrc"
posix = "$HOME/.bashrc"
linux = "$HOME/.bashrc.linux"

[monikers.apple-silicon]
when = "os = macos && arch = aarch64"

[monikers.posix]
when = "os in (linux, macos)"

[monikers.linux]
when = "os = linux"
~~~~

이 설정에서:

 -  `apple-silicon`의 구체성은 2입니다 (`&&`로 두 조건을 결합)
 -  `posix`의 구체성은 1입니다 (단일 `in` 검사)
 -  `linux`의 구체성은 1입니다 (단일 `=` 검사)

Apple Silicon Mac(aarch64 아키텍처의 macOS)에서는 `apple-silicon`과 `posix`
모두 일치합니다.  하지만 `apple-silicon`이 더 높은 구체성(2 > 1)을 가지므로
선택됩니다.

Intel Mac(x86\_64 아키텍처의 macOS)에서는 `posix`만 일치합니다.

Linux에서는 `posix`와 `linux` 모두 동일한 구체성으로 일치합니다.
이 경우 선언 파일에서의 순서가 동점 해결 기준이 됩니다.
서술어 비교에서 먼저 나타나는 쪽이 우선합니다.

### 실용적인 팁

 -  **일반적인 것부터 시작해 구체적인 것을 추가**: 먼저 넓은 범위의 라우팅
    (예: Unix 계열 시스템을 위한 `posix`)을 정의한 다음, 특정 환경을 위한
    더 구체적인 라우팅(예: `linux`, `apple-silicon`)을 추가하여 재정의합니다.

 -  **특수화에는 `&&` 사용**: 환경별 동작이 필요할 때는 `&&`로 조건들을 결합하여
    우선순위가 높은 더 구체적인 서술어를 만드세요.

 -  **겹치는 `||` 라우팅 피하기**: `||`를 사용하는 라우팅은 구체성이 크게
    증가하지 않습니다.  모호한 일치가 발생한다면, `&&` 조건으로 구조를
    재구성하는 것을 고려해 보세요.


공 라우팅
---------

어떤 설정 파일들은 환경에 따라 존재하지 않거나 필요하지 않을 수도 있습니다.
이럴 때는 아예 해당 환경에서는 라우팅을 하지 않을 수 있는데,
이를 **공 라우팅**(null routing)이라 합니다.  예를 들어, Windows에서만 필요한
설정 파일이 있다면 아래와 같이 라우팅할 수 있습니다.

~~~~ toml
[files."Microsoft.WindowsTerminal.json"]
win = "$LocalAppData/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"
~~~~

위 설정 파일은 Windows에서만 필요하므로, Linux나 macOS에서는 파일이 아예 생기지
않게 됩니다.

<!-- cSpell: ignore alacritty APPDATA ghci nvim psql PSQLRC 8wekyb3d8bbwe -->
