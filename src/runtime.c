#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int i)
{
   printf("%d\n", i);
}


void printString(char* s)
{
   printf("%s\n", s);
}

char* readString(void)
{
   char *ptr=NULL;
   size_t len;
   
   getline(&ptr, &len, stdin);
   
   return ptr;
}

int readInt(void)
{
   char *ptr=NULL;
   size_t len;
   int out;
   
   getline(&ptr, &len, stdin);
   out = atoi(ptr);
   free(ptr);
   return out;
}
  
void error(void)
{
   printf("runtime error\n");
}

char* __concat(char* a, char*b)
{
   char* out;
   out=malloc(strlen(a)+strlen(b)+1);
   strcpy(out,a);
   strcat(out,b);
   
   return out;
}
