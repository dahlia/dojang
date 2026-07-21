한국어 번역 용어집
==================

이 용어집은 Dojang의 영문 용어에 대응하는 표준 한국어 번역을 정합니다.  한국어
문서를 새로 쓰거나 고칠 때는 아래 번역을 일관되게 사용합니다.


번역 원칙
---------

 -  같은 개념은 문서와 문맥이 달라도 같은 번역어를 사용합니다.
 -  명령 이름, 옵션 이름, 식별자, 파일 이름, 리터럴 문법은 원문을 유지하고
    필요한 경우 코드 서식으로 표시합니다.
 -  표에 없는 프로젝트 용어를 번역해야 한다면 명확한 번역어를 정하고, 그
    번역어를 문서에 쓰는 변경에서 이 용어집도 함께 갱신합니다.
 -  한 영문 용어가 문맥에 따라 다른 뜻을 가질 때만 비고에 적힌 구분을 따릅니다.


용어
----

| English                      | 한국어                     | 비고                                                                                         |
| ---------------------------- | -------------------------- | -------------------------------------------------------------------------------------------- |
| alias                        | 별칭                       | 파일 시스템 경로의 별칭을 포함합니다.                                                        |
| application, apply           | 적용, 적용하다             | 명령 이름 `dojang apply`는 그대로 씁니다.                                                    |
| architecture                 | 프로세서 아키텍처          | `arch` 식별자는 그대로 씁니다.                                                               |
| baseline                     | 기준 사본                  | 고아 상태 판정에 쓰는 보존된 사본을 뜻합니다.                                                |
| checkout                     | 체크아웃                   |                                                                                              |
| codec                        | 코덱                       | 경로 규칙의 원본 바이트를 배치할 바이트로 변환합니다.                                        |
| codec cache                  | 코덱 캐시                  | 원문이나 렌더링 결과가 아니라 지문과 캐시 키를 상태에 저장합니다.                            |
| change key                   | 개정 키                    | `change-key` 필드 이름은 그대로 씁니다.                                                      |
| command                      | 명령                       | `command line`은 “명령행”으로 씁니다.                                                        |
| compact route                | 간결 경로 규칙             |                                                                                              |
| conflict                     | 충돌                       |                                                                                              |
| converge                     | 수렴하다                   |                                                                                              |
| deployment link              | 배포 링크                  | `kind = "symlink"` 경로 규칙이 배치하는 단방향 심볼릭 링크를 뜻합니다.                       |
| destination                  | 대상 경로                  | 동기화 대상을 뜻할 때 사용합니다. 복사나 마이그레이션의 도착점은 “목적지”로 쓸 수 있습니다.  |
| destination kind             | 대상 종류                  | `kind` 필드 값(`copy`, `symlink`)은 그대로 씁니다.                                           |
| detailed route               | 상세 경로 규칙             |                                                                                              |
| device                       | 기기                       | `device node`는 “장치 노드”로 씁니다.                                                        |
| directory                    | 디렉터리                   |                                                                                              |
| dry run                      | 모의 실행                  | 옵션 이름 `--dry-run`은 그대로 씁니다.                                                       |
| entry                        | 항목                       | 파일 시스템 항목과 상태 항목에 모두 사용합니다.                                              |
| environment                  | 환경                       |                                                                                              |
| environment facts            | 환경 정보                  | 키-값으로 다루는 기술 개념입니다. 일반 문장에서 참·거짓을 나타내는 `fact`는 “사실”로 씁니다. |
| environment predicate        | 환경 서술어                |                                                                                              |
| execution policy             | 실행 정책                  | `policy` 필드 값은 그대로 씁니다.                                                            |
| file path expression         | 파일 경로 표현식           |                                                                                              |
| filesystem                   | 파일 시스템                |                                                                                              |
| fingerprint                  | 지문                       |                                                                                              |
| first apply                  | 최초 적용                  | 훅 이름의 `first-apply`는 그대로 씁니다.                                                     |
| hook                         | 훅                         |                                                                                              |
| ignore pattern               | 무시 패턴                  |                                                                                              |
| intermediate snapshot        | 중간 스냅샷                | 공통 조상임을 강조할 때는 “공통 중간 스냅샷”으로 씁니다.                                     |
| lifecycle                    | 수명 주기                  |                                                                                              |
| machine                      | 머신                       | 머신 식별자와 머신 상태처럼 기술적인 상태 개념에 사용합니다. 사용자 장비는 “기기”로 씁니다.  |
| machine facts                | 머신 정보                  |                                                                                              |
| machine-local state          | 머신별 로컬 상태           |                                                                                              |
| machine state                | 머신 상태                  |                                                                                              |
| managed destination          | 관리 대상 경로             |                                                                                              |
| managed target               | 관리 대상                  |                                                                                              |
| manifest                     | 선언 파일                  | 짧은 문서 제목이나 링크에서는 “선언”으로 쓸 수 있습니다.                                     |
| manifest variable            | 선언 변수                  | `vars` 구획에서 정의하는 재사용 가능한 값을 뜻합니다.                                        |
| marker                       | 마커                       |                                                                                              |
| match                        | 일치, 일치하다             |                                                                                              |
| moniker                      | 모니커                     |                                                                                              |
| named pipe                   | 명명 파이프                |                                                                                              |
| null route                   | 공 경로 규칙               |                                                                                              |
| orphan, orphan record        | 고아, 고아 레코드          | 문맥에 따라 “고아 대상”, “고아 감지”처럼 씁니다.                                             |
| overwrite                    | 덮어쓰기, 덮어쓰다         |                                                                                              |
| portable mode                | 이식 가능한 모드           | `mode` 필드 값(`private` 등)은 그대로 씁니다.                                                |
| provenance                   | 결정 근거                  | 단순한 출처를 뜻할 때는 “출처”로 쓸 수 있습니다.                                             |
| record                       | 레코드, 기록하다           | 명사와 동사를 구분합니다.                                                                    |
| reflection, reflect          | 반영, 반영하다             | 명령 이름 `dojang reflect`는 그대로 씁니다.                                                  |
| reflection policy            | 반영 정책                  | 코덱 구현이 `identity`, `reject`, `re-add` 중 하나로 정합니다.                               |
| rendered bytes               | 렌더링된 바이트            | 코덱의 정방향 변환 결과이며 중간 스냅샷과 대상 경로에 배치됩니다.                            |
| registry                     | 레지스트리                 |                                                                                              |
| repository                   | 저장소                     |                                                                                              |
| route                        | 경로 규칙                  |                                                                                              |
| route branch                 | 경로 규칙 분기             |                                                                                              |
| route definition             | 경로 규칙 정의             |                                                                                              |
| route metadata               | 경로 규칙 메타데이터       |                                                                                              |
| route name                   | 경로 규칙 이름             |                                                                                              |
| route ownership              | 경로 규칙 소유권           | 문맥에 따라 “소유 경로 규칙”, “소유하다”처럼 씁니다.                                         |
| routing                      | 경로 지정                  |                                                                                              |
| secret codec                 | 비밀 값 코덱               | 비밀 값을 조회하거나 복호화하는 코덱을 뜻합니다.                                             |
| snapshot                     | 스냅샷                     |                                                                                              |
| socket                       | 소켓                       |                                                                                              |
| source, source file          | 원본, 원본 파일            | 사용자 문서에서는 “소스”를 쓰지 않습니다.                                                    |
| special file                 | 특수 파일                  |                                                                                              |
| specificity                  | 구체성                     |                                                                                              |
| standard output              | 표준 출력                  | `stdout` 식별자는 그대로 씁니다.                                                             |
| state record                 | 상태 레코드                |                                                                                              |
| state store                  | 상태 저장소                |                                                                                              |
| symbolic link                | 심볼릭 링크                |                                                                                              |
| symlink target               | 링크 대상                  | 심볼릭 링크가 가리키는 경로를 뜻합니다. 동기화 대상은 “대상 경로”로 씁니다.                  |
| target, target file          | 대상, 대상 파일            |                                                                                              |
| template codec               | 템플릿 코덱                | 순수한 템플릿 부분집합으로 UTF-8 원본을 렌더링합니다.                                        |
| traversal boundary           | 순회 경계                  | 링크로 배포된 디렉터리 경계를 뜻합니다.                                                      |
| unchanged, modified, missing | 변경 없음, 수정됨, 사라짐  | 상태 표시에 사용합니다.                                                                      |
| unmanage                     | 추적을 중단하다, 추적 해제 | 명령 이름 `dojang unmanage`는 그대로 씁니다.                                                 |
| worktree                     | 작업 트리                  |                                                                                              |
