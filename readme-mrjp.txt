Przedstawiam rozwiązanie zadania semestralnego, które wykonałem na wydziale
MIMUW w roku 2013, na przedmiocie MRJP - Metod Realizacji Języków Programowania.


Jest to kompilator języka LATTE do asemblera gnu, oraz w efekcie do
pliku binarnego ELF 32 bit i386.


Do zbudowania kompilatora potrzebne są:


Oprócz kompilatora haskella - ghc:

  sudo apt-get install ghc

do zbudowania potrzebne są też:

analizator leksykalny alex:

  sudo apt-get install alex

generator parserów happy:

  sudo apt-get install happy

oraz Backus Naur Form Converter, bnfc:

  sudo apt-get bnfc

To powinno wystarczyć do skompilowania programu głównego.


Aby użyć program, będący efektem tego projektu, potrzebne są jeszcze narzędzia,
głównie 32-bitowe libgcc i libc do kompilacji 32 bitowych plików binarnych:

  sudo apt-get install lib32gcc-7-dev
  sudo apt-get install libc6-dev-i386

Biblioteki są również niezbędne do skompilowania runtime.c do pliku 32 bitowego
 ( ELF 32 bit  dla procesora i386 )


Kompilacja, kompilator do asemblera gnu i386:

W głównym katalogu projektu wykonujemy:

make
gcc -c -m32 runtime.c

drugie polecenie przygotowuje bibliotekę z instrukcjami wbudowanymi języka.

Powinny powstać w tym katalogu dwa pliki:

latc_x86
runtime.o
  


W razie ewentualnych trudności proszę o kontakt. Mi udało się doprowadzić do
kompilacji projektu. i kompilacji przykładów:


kompilacja przykładów - uruchamiać z katalogu, w którym są pliki latc_x86
oraz runtime.o:


./latc_x86 plik_zrodlowy.lat


w efekcie, jeżeli wszystko poszło dobrze, w katalogu, gdzie był plik źródłowy
powinien powstać 32 bitowy plik wykonywalny a.out, oraz plik asemblera:
plik_zrodlowy.s
