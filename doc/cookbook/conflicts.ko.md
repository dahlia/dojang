충돌 다루기
===========

꽤 많은 프로그램이 설정 파일을 읽기만 하는 게 아니라 수정하기도 합니다.
때로는 사용자 역시 원본 설정 파일을 고치는 대신 깜빡하고 대상 설정 파일을
고칠 수도 있습니다.  이를 사용자가 금방 알아챘다면 즉시 `dojang reflect`
명령으로 해당 변경을 원본에 반영할 수 있지만, 이를 까먹고 원본 설정에
또 다른 변경을 가했다면 충돌이 발생할 수 있습니다.

일단 충돌이 감지되면 `dojang apply`는 경고를 출력하고 (`-f`/`--force` 옵션을
켜지 않은 한) 하려던 작업을 취소합니다.  충돌을 해결하려면 우선 어느 설정
파일이 충돌을 일으키는지 확인해야 합니다.  `dojang status` 명령을 실행하면
충돌을 일으키는 설정 파일을 확인할 수 있습니다.  예를 들어,
다음과 같은 결과를 볼 수 있습니다.

~~~~ console
$ dojang status
Source   ST Destination DT File
──────── ── ─────────── ── ──────────────────
modified F  modified    F  HOME/.bash_profile
~~~~

위 결과는 순서대로 다음을 뜻합니다.

 -  `modified`: 마지막 적용 (`dojang apply`) 이후 원본 설정 파일이 변경됨.
 -  `F`: 원본 설정 파일은 일반 파일임 (디렉토리나 심볼릭 링크가 아님).
 -  `modified`: 마지막 적용 이후 대상 설정 파일이 변경됨.
 -  `F`: 대상 설정 파일은 일반 파일임 (디렉토리나 심볼릭 링크가 아님).
 -  `HOME/.bash_profile`: 충돌을 일으키는 설정 파일의 원본 경로.

충돌을 일으키는 설정 파일을 확인했다면, 이제 해당 파일에서 어떤 부분이 충돌을
일으키는지 구체적으로 확인해야 합니다.  `dojang diff` 명령을 실행하면 충돌을
일으키는 설정 파일의 변경 내용을 확인할 수 있습니다.  예를 들어, 다음과 같은
결과를 볼 수 있습니다.

~~~~ console
$ dojang diff
--- ./HOME/.bash_profile
+++ /home/dahlia/.bash_profile
@@ -1,3 +1,3 @@
-# This line is changed from the source.
+# This line is changed from the destination.

~~~~

> **Tip**
>
> `dojang diff` 명령의 출력 형식은 `diff --unified` 명령과 기본적으로 같습니다.
> 만약 출력 형식을 다르게 하고 싶다면, `--diff-program` 옵션을 사용하세요.
> 예를 들어, `--diff-program delta` 옵션을 사용하면 [`delta`] 프로그램의 출력
> 형식으로 변경할 수 있습니다.

충돌을 일으키는 설정 파일의 변경 내용을 확인했다면, 이제 충돌을 해결할
차례입니다.  충돌을 해결하는 방법은 크게 세 가지가 있습니다.

 -  대상 파일의 변경 버리고 원본 파일의 변경만 취하기:
    `dojang apply -f` 명령을 실행하면 됩니다.
 -  원본 파일의 변경 버리고 대상 파일의 변경만 취하기:
    `dojang reflect -f` 명령을 실행하면 됩니다.
 -  원본 파일과 대상 파일의 변경 모두 취하기: 충돌을 일으키는 원본 설정 파일을
    직접 수정하고, `dojang apply -f` 명령을 실행하면 됩니다.

[`delta`]: https://github.com/dandavison/delta
