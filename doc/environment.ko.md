환경
====

> [!NOTE]
>
> **환경**은 **환경 변수**(environment variable)와는 전혀 다른 개념입니다.

**환경**(environment)은 설정 파일들을 적용할 여러 컴퓨터들을 분간하긴 위한
기본적인 속성들로, 현재로서는 **운영체제**(`os`), **프로세서 아키텍처**(`arch`),
**커널**(`kernel`) 정보 세 가지로 이뤄져 있습니다.[^1]  예를 들어,
평범한 Windows PC는 아마도 아래와 유사한 환경을 갖췄을 것입니다.

~~~~ toml
os = "windows"
arch = "x86_64"  # Intel 또는 AMD 계열의 64비트 CPU

[kernel]
name = "Microsoft Windows"
release = "10.0.23585.1001"  # Windows 11 22H2
~~~~

혹은 2022년 이후에 구매한 MacBook이라면 아마 아래의 환경을 갖췄을 것입니다.

~~~~ toml
os = "macos"
arch = "aarch64"  # Apple silicon

[kernel]
name = "Darwin"
release = "23.1.0"  # macOS 14.1
~~~~

[^1]: 미래에 더 추가될 수 있습니다. 이를테면 운영체제 버전이 들어갈 수 있습니다.


현재 환경 확인하기
------------------

Dojang을 사용하기에 앞서, 현재 쓰고 있는 기기가 어떤 환경으로 인식되는지
`dojang env` 명령으로 알아볼 수 있습니다.

~~~~ console
$ dojang env
os = "linux"
arch = "x86_64"

[kernel]
name = "Linux"
release = "5.10.0-8"
~~~~


인식하는 운영체제 목록
----------------------

다음은 Dojang이 인식하는 운영체제들과 Dojang에서 이들을 가리키는 키워드를 나열한
것입니다.[^2]

 -  Android: `android`
 -  FreeBSD: `freebsd`
 -  GNU/Linux: `linux`
 -  macOS (Mac OS X 이후의 모든 버전): `macos`
 -   NetBSD: `netbsd`
 -  OpenBSD: `openbsd`
 -  Windows: `windows`

[^2]: 앞으로 더 추가될 수 있습니다. 특히, 원하신다면 Dojang 프로젝트에 기여해서
직접 추가하실 수 있습니다.


인식하는 프로세서 아키텍처 목록
-------------------------------

다음은 Dojang이 인식하는 프로세서 아키텍처들과 Dojang에서 이들을 가리키는
키워드를 나열한 것입니다.[^2]

 -  ARM64: `aarch64`
 -  Intel 및 AMD 계열 32비트: `x86`
 


커널 인식
---------

Dojang은 현재 실행되는 기기의 커널 정보를 인식합니다.  커널 정보는 POSIX 계열의
경우 `uname -sr` 명령의 출력을 사용하고, Windows의 경우 `ver` 명령의 출력을
사용합니다.


직접 환경 지정하기
------------------

Dojang은 알아서 현재 실행되는 기기가 어떤 환경인지 인식하지만, 인식된 환경이
아닌 다른 환경인 것처럼 가장하고 싶을 수 있습니다.
특히, 다른 기기에서 어떻게 동작할지 테스트하고 싶을 때가 그렇습니다.
*dojang-env.toml* 파일[^1]을 *dojang.toml* 파일 옆에 두는 것으로 Dojang이 직접
인식한 환경 대신 그 파일 안에 지정된 환경으로 인식하게 할 수 있습니다.
파일 형식은 `dojang env`의 결과 형식과 같습니다.

~~~~ toml
os = "linux"
arch = "aarch64"

[kernel]
name = "Linux"
release = "6.5.9-300.fc35.aarch64"
~~~~

참고로, `dojang env -o dojang-env.toml` 명령으로 *dojang-env.toml* 파일의
샘플을 만든 뒤, 그 파일을 고치는 식으로 작업할 수도 있습니다.

[^1]: `-e`/`--env-file` 옵션을 사용하면 *dojang-env.toml* 파일 대신 다른 파일을
      사용할 수 있습니다.
