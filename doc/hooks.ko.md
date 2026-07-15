훅
==

**훅**(hook)을 사용하면 `dojang apply` 명령 전후에 사용자 정의 스크립트를 실행할
수 있습니다.  다음과 같은 작업에 유용합니다:

 -  설정 파일 적용 전 의존성 설치
 -  설정 변경 후 서비스 재시작
 -  최초 설치 시 설정 스크립트 실행
 -  임시 파일 정리


훅 종류
-------

네 가지 종류의 훅이 있으며, 다음 순서로 실행됩니다:

| 훅 종류            | 실행 시점                       |
| ------------------ | ------------------------------- |
| `pre-apply`        | 매 적용의 파일 동기화 직전      |
| `pre-first-apply`  | 최초 적용의 파일 동기화 직전    |
| `post-first-apply` | 최초 적용의 파일 동기화 완료 후 |
| `post-apply`       | 매 적용의 파일 동기화 완료 후   |

**최초 적용**은 저장소별·머신별 로컬 상태에 기록됩니다.  실패한 적용과 모의
실행은 이 상태를 소비하지 않습니다.  체크아웃을 옮겨도 초기화되지 않지만,
`dojang forget`을 실행하면 초기화됩니다. `dojang apply`를 처음 실행하면 네 가지
훅이 모두 다음 순서로 실행됩니다:

1.  `pre-apply`
2.  `pre-first-apply`
3.  *(파일 동기화)*
4.  `post-first-apply`
5.  `post-apply`

이후 적용에서는 `pre-apply`와 `post-apply`만 실행됩니다:

1.  `pre-apply`
2.  *(파일 동기화)*
3.  `post-apply`


훅 정의하기
-----------

훅은 *dojang.toml* 선언 파일의 `hooks` 구획에서 정의합니다.  각 훅 종류는
TOML의 테이블 배열 문법(`[[hooks.TYPE]]`)을 사용합니다:

~~~~ toml
[[hooks.pre-apply]]
command = "/bin/echo"
args = ["설정 중..."]

[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "restart", "my-service"]
~~~~

같은 종류의 훅을 여러 개 정의할 수 있습니다.  정의된 순서대로 실행됩니다.

### 훅 필드

 -  `command` (문자열, 필수): 실행할 실행 파일 경로.  절대 경로이거나
    `$PATH`에서 찾을 수 있는 명령이어야 합니다.

 -  `args` (문자열 배열, 선택): 명령에 전달할 인자.  기본값은 빈 배열.

 -  `when` (문자열, 선택): [환경 서술어](environment-predicate.ko.md) 표현식.
    조건을 만족할 때만 훅이 실행됩니다.  기본값은 항상 실행.

 -  `working-directory` (문자열, 선택): 훅의 작업 디렉터리.  기본값은 저장소
    디렉터리.

 -  `ignore-failure` (불리언, 선택): `true`이면 훅이 0이 아닌 종료 코드로
    끝나도 Dojang이 계속 진행합니다.  기본값은 `false`.

### 조건부 훅

`when` 필드를 사용해 특정 환경에서만 훅을 실행할 수 있습니다:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "restart", "dunst"]
when = "os = linux"

[[hooks.post-apply]]
command = "/usr/bin/killall"
args = ["NotificationCenter"]
when = "os = macos"
~~~~


환경 변수
---------

훅에서 사용할 수 있는 환경 변수:

| 변수                | 설명                          |
| ------------------- | ----------------------------- |
| `DOJANG_REPOSITORY` | 저장소 디렉터리의 절대 경로   |
| `DOJANG_MANIFEST`   | 선언 파일(*dojang.toml*) 경로 |
| `DOJANG_DRY_RUN`    | 모의 실행이면 `1`, 아니면 `0` |
| `DOJANG_OS`         | 현재 운영체제 식별자          |
| `DOJANG_ARCH`       | 현재 프로세서 아키텍처 식별자 |


모의 실행
---------

`dojang apply --dry-run` 실행 시 훅은 실행되지 않습니다.  대신 Dojang이 실행될
내용을 출력합니다:

~~~~
Note: Would run hook: /bin/echo 설정 중...
~~~~


오류 처리
---------

훅이 0이 아닌 종료 코드로 끝나면 Dojang은 중단되고 종료 코드 40
(`hookFailedError`)으로 종료합니다.  훅 실패에도 계속 진행하려면
`ignore-failure = true`를 사용하세요:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/optional-command"
args = []
ignore-failure = true
~~~~


예시
----

### 최초 설치 설정

최초 적용에서만 설정 스크립트 실행:

~~~~ toml
[[hooks.pre-first-apply]]
command = "/bin/bash"
args = ["-c", "echo '최초 설정!' && ./setup.sh"]
~~~~

### 플랫폼별 훅

운영체제에 따라 다른 명령 실행:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/brew"
args = ["bundle", "--file=Brewfile"]
when = "os = macos"

[[hooks.post-apply]]
command = "/usr/bin/apt"
args = ["install", "-y", "packages..."]
when = "os = linux"
~~~~

### 서비스 재시작

설정 변경 후 서비스 재시작:

~~~~ toml
[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "reload", "my-app"]
when = "os = linux"
~~~~

<!-- cSpell:ignore dunst killall Brewfile -->
