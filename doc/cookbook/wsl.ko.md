WSL 감지
========

> **Tip**
>
> WSL은 Windows Subsystem for Linux의 약자로, Windows에서 리눅스를 사용할 수
> 있게 해주는 기능입니다.  자세한 내용은 [WSL 공식
> 문서](https://docs.microsoft.com/windows/wsl/)를 참고하세요.

> **Note**
>
> 이 문서는 WSL 2를 기준으로 작성되었습니다.  WSL 1에서는 동작하지 않을 수
> 있습니다.  (문서 기여는 환영.)

WSL은 Dojang에서 일반적인 Linux 환경으로 감지됩니다.  하지만, 만약 일반 Linux
환경과 달리 WSL 환경에서만 특수하게 처리하고 싶은 설정 파일이 있다면,
일반적인 Linux와 WSL을 구분할 수 있어야 합니다.

현재로서 WSL을 감지하는 유일한 방법은 `uname -r`이나 */proc/version*의 내용에
`-microsoft-standard-WSL2`라는 문구가 들어있는지 확인하는 것으로 보입니다.
다행히, Dojang은 환경을 감지할 때 `kernel-release` 요소를 함께 인식하므로,
[선언](../manifest.ko.md) 파일에 다음과 같이 WSL에서만 만족되는
[모니커](../manifest.ko.md#모니커)를 정의할 수 있습니다.

~~~~ toml
[monikers.wsl]
when = "kernel-release $= '-microsoft-standard-WSL2'"
~~~~

일반적인 문자열 일치 비교가 아니라 `$=` (…로 끝남) 연산자를 사용해야 하기
때문에 `when` 필드 안에 [환경 서술자 문법](../environment-predicate.ko.md)을
사용해야 한다는 점에 주의하세요.
