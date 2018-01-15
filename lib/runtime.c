//#include<string.h>
void __printInt(int x) {
	printf("%d\n", x);
}

void __printString(char* s) {
	printf("%s\n", s);
}
typedef struct _iobuf
{
    char*   _ptr;
    int _cnt;
    char*   _base;
     int _flag;
     int _file;
     int _charbuf;
     int _bufsiz;
     char*   _tmpfname;
} FILE;

extern FILE* stdin;
char* __readString() {
    int default_size = 128;
	char* ret ;
    ret = malloc (default_size + 1);
    fgets(ret, default_size, stdin);
	//getline(&ret, &default_size, stdin);
	//fgets(ret, default_size, 0);
	//gets(&ret);
	//scanf( "%128[^\n]", &ret);
    ret[strlen(ret) - 1] = '\0';
    
    return ret;
}

int __readInt() {
	int x;
	scanf("%d\n",&x);

	return x;
}

extern int _main();

char* __concat(char* s1, char* s2) {
	unsigned int l1 = strlen(s1);
	unsigned int l2 = strlen(s2);
	char* ret = malloc(l1 + l2 + 1);
	if (ret == 0) {
	        perror("malloc");
	}
	ret[l1+l2] = 0;
	memcpy(ret, s1, l1);
	memcpy(ret+l1, s2, l2);
	return ret;
}

void _start() {
        exit(_main());
}

char *__new_str(char *str) {
	int len = strlen(str);
	char *mem = malloc(len + 1); // if mem == NULL to sie wywalilo
	memcpy(mem, str, len+1);
	return mem;
}

void __error() {
	printf("runtime error\n");
	exit(1);
}
