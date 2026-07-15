훅
==

훅은 `apply`, `reflect`, `diff`, `status`, `edit`, `unmanage` 명령의 수명 주기
전후에 사용자 정의 명령을 실행합니다.  명령 실행 전 도구를 준비하거나, 변경에
성공한 뒤 서비스를 다시 불러오거나, 머신마다 한 번만 설정할 때 사용할 수
있습니다.


수명 주기 이벤트
----------------

지원하는 명령마다 `pre-*` 이벤트와 `post-*` 이벤트가 있습니다.  사후 훅은
명령이 성공했을 때만 실행됩니다.

| 명령       | 사전 이벤트    | 성공 시 사후 이벤트 |
| ---------- | -------------- | ------------------- |
| `apply`    | `pre-apply`    | `post-apply`        |
| `reflect`  | `pre-reflect`  | `post-reflect`      |
| `diff`     | `pre-diff`     | `post-diff`         |
| `status`   | `pre-status`   | `post-status`       |
| `edit`     | `pre-edit`     | `post-edit`         |
| `unmanage` | `pre-unmanage` | `post-unmanage`     |

`reflect`, `diff`, `status`, `edit`, `unmanage`는 사전 훅을 실행한 뒤 명령
동작에 사용할 *dojang.toml*과 *dojang-env.toml*을 다시 읽습니다.  성공 시 사후
훅을 선택하기 전에도 같은 컨텍스트를 다시 읽으므로, 이전 선언 파일이나 머신
환경을 바탕으로 사후 훅을 실행하지 않습니다.

`apply`는 `pre-first-apply`와 `post-first-apply`도 지원합니다.  저장소와 머신의
최초 적용에서는 다음 순서로 실행됩니다:

1.  `pre-apply`
2.  `pre-first-apply`
3.  파일 동기화
4.  `post-first-apply`
5.  `post-apply`

실패한 적용과 모의 실행은 최초 적용 상태를 소비하지 않습니다.  체크아웃을
옮겨도 초기화되지 않지만 `dojang forget`을 실행하면 초기화됩니다.

`init`, `migrate`, `env`, `forget`에는 훅이 실행되지 않습니다.


훅 정의하기
-----------

*dojang.toml*의 `hooks` 구획에 훅을 정의합니다.  수명 주기 이벤트에는 TOML의
테이블 배열 문법을 사용합니다:

~~~~ toml
[[hooks.pre-reflect]]
command = "/usr/bin/make"
args = ["prepare"]

[[hooks.post-apply]]
command = "/usr/bin/systemctl"
args = ["--user", "reload", "my-service"]
when = "os = linux"
~~~~

같은 이벤트의 훅은 선언 파일에 적힌 순서대로 실행됩니다.  상태 저장형 훅 ID는
한 이벤트 안에서 중복될 수 없습니다.

### 필드

 -  `command` (문자열, 필수): 실행 파일 경로 또는 `PATH`에서 찾을 수 있는 명령.
 -  `args` (문자열 배열): 명령 인자.  기본값은 빈 배열.
 -  `when` (문자열): 훅을 검토하기 전에 만족해야 하는 [환경 서술어].  기본값은
    `always`.
 -  `working-directory` (문자열): 작업 디렉터리.  기본값은 저장소 디렉터리.
 -  `ignore-failure` (불리언): 실행 실패나 0이 아닌 종료 코드 뒤에도 명령을
    계속 진행할지 여부.  기본값은 `false`.
 -  `id` (문자열): 상태 저장형 훅의 안정적인 식별자.
    `[A-Za-z][A-Za-z0-9._-]*`와 일치해야 합니다.
 -  `policy` (문자열): `always`, `once`, `on-change` 중 하나.  기본값은
    `always`.
 -  `change-key` (비어 있지 않은 문자열): `on-change`에 필요한 명시적인 개정 키.

[환경 서술어]: environment-predicate.ko.md


실행 정책
---------

`always`는 이벤트가 발생하고 `when` 서술어가 일치할 때마다 실행됩니다.  `id`는
선택 사항이며 성공 기록을 저장하지 않습니다.

`once`에는 `id`가 필요합니다.  이 저장소와 머신에서 한 번 성공할 때까지
실행됩니다:

~~~~ toml
[[hooks.pre-status]]
id = "install-status-helper"
policy = "once"
command = "/usr/local/bin/install-status-helper"
~~~~

`on-change`에는 `id`와 `change-key`가 모두 필요합니다.  명시적인 키, 정규화된
훅 설정, 일치하는 머신 정보, 수명 주기 이벤트, 선택한 경로 범위 중 하나가
바뀌면 실행됩니다:

~~~~ toml
[[hooks.post-apply]]
id = "rebuild-cache"
policy = "on-change"
change-key = "cache-format-v3"
command = "/usr/local/bin/rebuild-cache"
~~~~

지문은 관리 파일 내용이나 선언 파일 전체를 해시하지 않습니다.  Dojang이 알 수
없는 방식으로 스크립트 동작이 바뀌면 `change-key`를 변경하세요.  기록은
저장소별·머신별 로컬 상태이므로 체크아웃을 옮겨도 성공한 `once` 훅이 다시
실행되지 않습니다.

성공한 실행만 기록합니다.  `ignore-failure = true`로 허용한 실패는 명령을 계속
진행하지만 다음에도 실행 대상입니다.


훅 환경
-------

훅은 부모 프로세스 환경을 상속하며 Dojang이 다음 값을 덮어씁니다:

| 변수                        | 값                            |
| --------------------------- | ----------------------------- |
| `DOJANG_REPOSITORY`         | 저장소 절대 경로              |
| `DOJANG_MANIFEST`           | 선언 파일 절대 경로           |
| `DOJANG_REPOSITORY_ID`      | 저장소 UUID                   |
| `DOJANG_MACHINE_STATE`      | 저장소 머신 상태 파일         |
| `DOJANG_INTERMEDIATE`       | 중간 스냅샷 경로              |
| `DOJANG_COMMAND`            | 명령 이름                     |
| `DOJANG_HOOK_EVENT`         | 수명 주기 이벤트              |
| `DOJANG_HOOK_ID`            | 안정적이거나 파생된 훅 식별자 |
| `DOJANG_HOOK_POLICY`        | 실행 정책                     |
| `DOJANG_DRY_RUN`            | 실행되는 훅에서는 `0`         |
| `DOJANG_OS` / `DOJANG_ARCH` | 운영체제와 아키텍처           |
| `DOJANG_KERNEL`             | 커널 이름                     |
| `DOJANG_KERNEL_RELEASE`     | 커널 릴리스                   |
| `DOJANG_PATH_COUNT`         | 선택 범위 경로 수             |
| `DOJANG_PATH_0`, …          | 안정적인 순서의 범위 경로     |

`reflect --source`, `edit --source`, `unmanage --route`로 전달한 라우트 선택자는
저장소 상대 경로로 유지됩니다.  그 밖의 상대 경로 인자는 호출한 작업 디렉터리를
기준으로 합니다.  따라서 `-r`로 저장소를 선택해도 라우트 선택자의 범위나 지문은
달라지지 않습니다.

사후 훅에는 `DOJANG_COMMAND_OUTCOME=success`와 `DOJANG_EXIT_CODE=0`도
전달합니다.  Dojang은 현재 문맥을 추가하기 전에 상속받은 훅 문맥 변수를
제거합니다.  따라서 사전 훅의 사후 명령 결과나 `DOJANG_PATH_COUNT`를 초과하는
경로 인덱스처럼 현재 이벤트에서 생략한 값은 설정되지 않은 상태로 남습니다.


모의 실행과 재귀
----------------

모의 실행은 실행 대상 훅의 수명 주기 이벤트, 실제 작업 디렉터리, 정책상 실행
이유를 출력합니다.  프로세스를 시작하거나 훅 잠금을 얻거나 실행 기록을
갱신하지 않습니다.

훅이 시작한 Dojang 프로세스는 기본적으로 자체 훅을 억제합니다.  바깥 호출에
`--allow-hook-recursion`을 전달하면 한 단계 안쪽 호출의 훅을 허용합니다.  재귀
식별자에는 저장소 ID가 들어가므로 다른 저장소의 같은 이름을 가진 훅은 실행할 수
있습니다.  두 번째 안쪽 단계는 계속 억제되며 같은 저장소의 상위 훅은 자신을 다시
진입할 수 없습니다.


실패와 동시 실행
----------------

실행 실패나 0이 아닌 종료 코드는 `ignore-failure = true`가 아니면 Dojang을
중단하고 종료 코드 40(`hookFailedError`)을 반환합니다.  비동기 중단은 무시한 훅
실패로 처리하지 않습니다.

저장소, 이벤트, `id`가 같은 상태 저장형 훅은 여러 프로세스 사이에서
직렬화됩니다. 훅 잠금을 보유한 상태에서 실행 대상을 다시 확인하며, 성공 기록은
저장소의 원자적 상태 갱신으로 저장합니다.  훅 실행 중 저장소 상태를 잊고 다시
만들면 이전 실행 기록은 새 상태 세대에 들어가지 않습니다.  실행 기록을 쓰기
전에 훅 ID를 다시 검증합니다.

<!-- cSpell:ignore systemctl -->
