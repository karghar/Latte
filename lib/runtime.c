#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern void printInt(int x) {
	printf("%d\n", x);
}

extern void printString(char* s) {
	printf("%s\n", s);
}


char* readString() {
        char* ret;
        int default_size = 128;
        ret = malloc (default_size + 1);
        getline(&ret, &default_size, stdin);
        ret[strlen(ret) - 1] = 0;
        return ret;
}

int readInt() {
	int x;
	char* str = readString();
	sscanf(str, "%d", &x);

	return x;
}

char* __concat(char* s1, char* s2) {
	char* ret = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(ret, s2);
	strcat(ret, s1);
	return ret;
}

char *new_str(char *str) {
	size_t len = strlen(str);
	char *mem = malloc(len + 1); // if mem == NULL to sie wysralo
	memcpy(mem, str, len+1);
	return mem;
}

void error() {
	printf("runtime error\n");
	exit(1);
}
