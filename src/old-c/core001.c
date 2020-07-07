typedef char* string;

string __concat(string a, string b);
void printInt(int);
void printString(string);

string repStr(string s, int n) {
  string r = "";
  int i = 0;
  while(i<n) {
    r = __concat(r, s);
    i++;
  }
 return r;
}

int fac(int a) {
	int r;
	int n;

	r = 1;
	n = a;
	while (n > 0) {
	  r = r * n;
	  n = n - 1;
	}
	return r;
}

int rfac(int n) {
	if (n == 0)
	  return 1;
	else
	  return n * rfac(n-1);
}


int nfac(int);

int mfac(int n) {
	if (n == 0)
	  return 1;
	else
	  return n * nfac(n-1);
}

int nfac(int n) {
	if (n != 0)
	  return mfac(n-1) * n;
	else
	  return 1;
}

int ifac2f(int l, int h) {
        if (l == h)
          return l;
        if (l > h)
          return 1;
        int m;
        m = (l + h) / 2;
        return ifac2f(l,m) * ifac2f(m+1,h);
}

int ifac(int n) { return ifac2f(1,n); }

int main() {
	printInt(fac(10));
	printInt(rfac(10));
	printInt(mfac(10));
        printInt(ifac(10));
        string r="" ; // just to test blocks 
	{
	  int n = 10;
	  int r = 1;
	  while (n>0) {
	    r = r * n;
	    n--;
	  }
	  printInt(r);
	}
	printString (repStr("=",60));
	printString ("hello */");
        printString ("/* world") ;
        return 0 ;
}

