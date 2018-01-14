#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void __printInt(int x) {
	printf("%d\n", x);
}

void __printString(char* s) {
	printf("%s\n", s);
}


char* __readString() {
        char* ret;
        int default_size = 128;
        ret = malloc (default_size + 1);
        getline(&ret, &default_size, stdin);
        ret[strlen(ret) - 1] = 0;
        return ret;
}

int __readInt() {
	int x;
	char* str = __readString();
	sscanf(str, "%d", &x);

	return x;
}

char* __concat(char* s1, char* s2) {
	char* ret = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(ret, s2);
	strcat(ret, s1);
	return ret;
}

char *__new_str(char *str) {
	size_t len = strlen(str);
	char *mem = malloc(len + 1); // if mem == NULL to sie wysralo
	memcpy(mem, str, len+1);
	return mem;
}

void __error() {
	printf("runtime error\n");
	exit(1);
}
