종료 코드
=========

`dojang` 명령어는 오류로 인해 종료될 때 상황에 따라 아래와 같은 종료 코드를
반환합니다.

 -  -1: 예기치 못한 오류.
 -  1: 잘못된 명령어 옵션 또는 인자.
 -  2: 파일 쓰기 실패.
 -  3: 파일이 없음.
 -  4: 외부 프로그램이 오류로 종료됨 (0이 아닌 종료 코드).
 -  9: 해당 환경(플랫폼)에서 지원하지 않음.
 -  10: 선언 파일이 없거나 저장소가 초기화되지 않음.
 -  11: 선언 파일을 잘못된 형식이나 권한 등의 문제로 읽을 수 없음.
 -  12: 선언 파일이 이미 있어서 새로 만들 수 없음.
 -  20: 환경 파일이 없음.
 -  21: 환경 파일을 잘못된 형식이나 권한 등의 문제로 읽을 수 없음.
 -  30: 저장소와 적용 대상 사이에 충돌하는 변경이 각자 있어서 변경을 적용할 수
    없음.
 -  31: 저장소 내의 파일은 대상 파일이 될 수 없음.
 -  32: 라우팅되지 않은 파일이라 작업을 수행할 수 없음.
 -  33: 무시된 파일이라 작업을 수행할 수 없음.
 -  34: 대상 파일 중 일부가 삭제될 수 있어서 작업을 취소함.