all : latc_x86

latc_x86 :
	cd src/bnfc;./mkBnfc;cd ..;./make;mv latc_x86 ..;cd ..
