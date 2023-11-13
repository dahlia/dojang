크로스플랫폼 *.gitconfig*
=========================

Dojang을 이용하면 여러 기기에서 거의 같은 Git 설정을 사용하는 동시에,
환경에 따라 일부 설정을 다르게 할 수 있습니다.
이를테면 이름이나 자주 쓰는 명령어 별명 등은 모든 기기에서 동일하게 사용하고,
편집기나 SSH, GnuPG 프로그램 등은 기기마다 다르게 설정할 수 있습니다.

예를 들어, macOS에서는 다음과 같은 설정을 쓰고 싶고…

~~~~ gitconfig
[user]
	name = 홍길동
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
[core]
	editor = vim
[gpg]
	path = /usr/local/bin/gpg
~~~~

Windows에서는 다음과 같은 설정을 쓰고 싶다면…

~~~~ gitconfig
[user]
	name = 홍길동
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
[core]
	editor = notepad
	sshCommand = C:/Windows/System32/OpenSSH/ssh.exe
	autocrlf = false
	symlinks = true
[credential "helperselector"]
	selected = manager
[gpg]
	program = C:\\Program Files (x86)\\GnuPG\\bin\\gpg.exe
~~~~

어떻게 해야 할까요?


환경에 따라 내용 달라지게 하기
------------------------------

겹치는 설정을 한 곳에 모으는 작업은 나중에 하기로 하고,
우선 환경에 따라 설정이 달라지게 하겠습니다.
이를 위해선 파일 단위 라우팅을 통해 환경별 설정 파일이 *~/.gitconfig*에
복사되도록 해야 합니다.  [선언](../manifest.ko.md)(*dojang.toml*) 파일에
다음과 같이 설정합니다.

~~~~ toml
[files."gitconfig/.gitconfig.mac"]
mac = "$HOME/.gitconfig"

[files."gitconfig/.gitconfig.win"]
win = "$UserProfile/.gitconfig"

[monikers.mac]
os = "macos"

[monikers.win]
os = "windows"
~~~~

환경 하나에 하나의 파일 단위 라우팅을 만든 것에 주목하세요.
*gitconfig/.gitconfig.mac*의 라우팅을 보면 `mac` 모니커를 만족할 경우의
경로만 적혀 있고, `win` 모니커가 만족했을 때의 경로는 누락되어 있습니다.
이렇게 의도적으로 특정 환경에 대해서 누락시키는 것을
[공 라우팅](../routing.ko.md#공-라우팅)이라고 합니다.
공 라우팅을 사용하면 특정 환경에 대해서는 라우팅이 존재하지 않는 것으로
간주되어, 해당 환경에 대한 설정 파일이 복사되지 않습니다.

위와 같은 설정을 통해, macOS에서는 *gitconfig/.gitconfig.mac*의 내용이
*~/.gitconfig*에 복사되고, Windows에서는 *gitconfig/.gitconfig.win*의 내용이
*~/.gitconfig*에 복사되게 됩니다.


공통 설정 파일 만들기
---------------------

공통 설정을 모아두는 것은 Git 설정 자체에서 지원되는 `[include]` 기능을 쓰면
간단히 해결됩니다.  *gitconfig/.gitconfig.mac*을 다음과 같이 만들어 줍니다.

~~~~ gitconfig
[include]
	path = ~/.gitconfig.common
[core]
	editor = vim
[gpg]
	path = /usr/local/bin/gpg
~~~~

*gitconfig/.gitconfig.win*은 다음과 같이 만들어 줍니다.

~~~~ gitconfig
[include]
	path = ~/.gitconfig.common
[core]
	editor = notepad
	sshCommand = C:/Windows/System32/OpenSSH/ssh.exe
	autocrlf = false
	symlinks = true
[credential "helperselector"]
	selected = manager
[gpg]
	program = C:\\Program Files (x86)\\GnuPG\\bin\\gpg.exe
~~~~

마지막으로, 공통 설정 파일인 *gitconfig/.gitconfig.common*을 만들어 줍니다.

~~~~ gitconfig
[user]
	name = 홍길동
[alias]
	f = fetch --tags --all -p
	ci = commit
	co = switch --create
	st = status --short --branch
~~~~

*gitconfig/.gitconfig.common* 파일이 *~/.gitconfig.common*에 복사되도록 선언
파일에 아래와 같이 파일 단위 라우팅을 하나 더 추가하는 것을 잊으면 안 됩니다.

~~~~ toml
[files."gitconfig/.gitconfig.common"]
mac = "$HOME/.gitconfig.common"
win = "$UserProfile/.gitconfig.common"
~~~~

<!-- cSpell:ignore autocrlf helperselector -->
