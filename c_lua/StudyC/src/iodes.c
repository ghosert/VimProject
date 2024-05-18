#include <stdio.h>
int main(void) {
	FILE * fp;
	if ((fp = fopen("ttt", "r+")) == NULL) exit(1);
	int i = fgetc(fp);
	printf("%d", i);
	fflush(fp); // this function is neccessary for "r+" pattern, means push the data in cache to file at once.
	i = fputc('z', fp);
	if (i == EOF) printf("error happened.");
	fclose(fp);
	
//	fgetc（由文件中读取一个字符）  
//	定义函数  int fgetc(FILE * stream);
//	函数说明  fgetc()从参数stream所指的文件中读取一个字符。若读到文件尾而无数据时便返回EOF。
//	same to getc() getc() is a macro definition
//	
//	fputc（将一指定字符写入文件流中）  
//	定义函数  int fputc(int c,FILE * stream);
//	返回值  fputc()会返回写入成功的字符，即参数c。若返回EOF则代表写入失败。
//	same to putc() putc() is a macro definition
//	
//	fgets（由文件中读取一行字符串）  
//	定义函数  char * fgets(char * s,int size,FILE * stream);
//	函数说明  fgets()用来从参数stream所指的文件内读入字符并存到参数s所指的内存空间，直到出现换行字符、读到文件尾或是已读了size-1个字符为止，最后会加上NULL作为字符串结束。
//	 
//	fputs（将一指定的字符串写入文件内）  
//	定义函数  int fputs(const char * s,FILE * stream);
//	函数说明  fputs()用来将参数s所指的字符串写入到参数stream所指的文件内。
//	返回值  若成功则返回写出的字符个数，返回EOF则表示有错误发生。
//	
//	fileno（返回文件流所使用的文件描述词）  
//	定义函数  int fileno(FILE * stream);
//	函数说明  fileno()用来取得参数stream指定的文件流所使用的文件描述词。return fd;
//	 
//	fopen（打开文件）  
//	定义函数  FILE * fopen(const char * path,const char * mode);
//	mode有下列几种形态字符串:
//	r 打开只读文件，该文件必须存在。
//	r+ 打开可读写的文件，该文件必须存在。
//	w 打开只写文件，若文件存在则文件长度清为0，即该文件内容会消失。若文件不存在则建立该文件。
//	w+ 打开可读写文件，若文件存在则文件长度清为零，即该文件内容会消失。若文件不存在则建立该文件。
//	a 以附加的方式打开只写文件。若文件不存在，则会建立该文件，如果文件存在，写入的数据会被加到文件尾，即文件原先的内容会被保留。
//	a+ 以附加方式打开可读写的文件。若文件不存在，则会建立该文件，如果文件存在，写入的数据会被加到文件尾后，即文件原先的内容会被保留。
//	上述的形态字符串都可以再加一个b字符，如rb、w+b或ab＋等组合，加入b 字符用来告诉函数库打开的文件为二进制文件，而非纯文字文件。不过在POSIX系统，包含Linux都会忽略该字符。由fopen()所建立的新文件会具有S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH(0666)权限，此文件权限也会参考umask值。
//	返回值  文件顺利打开后，指向该流的文件指针就会被返回。若果文件打开失败则返回NULL，并把错误代码存在errno 中。
//	 
//	fread（从文件流读取数据）  
//	定义函数  size_t fread(void * ptr,size_t size,size_t nmemb,FILE * stream);
//	函数说明  fread()用来从文件流中读取数据。参数stream为已打开的文件指针，参数ptr 指向欲存放读取进来的数据空间
//	读取的字符数以参数size*nmemb来决定。Fread()会返回实际读取到的nmemb数目，如果此值比参数nmemb 来得小
//	则代表可能读到了文件尾或有错误发生，这时必须用feof()或ferror()来决定发生什么情况。
//	返回值  返回实际读取到的nmemb数目。
//	 
//	fseek（移动文件流的读写位置）  
//	定义函数  int fseek(FILE * stream,long offset,int whence);
//	函数说明  fseek()用来移动文件流的读写位置。参数stream为已打开的文件指针，参数offset为根据参数whence来移动读写位置的位移数。
//	返回值  当调用成功时则返回0，若有错误则返回-1，errno会存放错误代码。
//	附加说明  fseek()不像lseek()会返回读写位置，因此必须使用ftell()来取得目前读写的位置。
//	范例  #include<stdio.h>
//	main()
//	{
//	FILE * stream;
//	long offset;
//	fpos_t pos;
//	stream=fopen(“/etc/passwd”,”r”);
//	fseek(stream,5,SEEK_SET);
//	printf(“offset=%d\n”,ftell(stream)); // ftell return -f if error
//	rewind(stream);
//	fgetpos(stream,&pos);
//	printf(“offset=%d\n”,pos);
//	pos=10;
//	fsetpos(stream,&pos);
//	printf(“offset = %d\n”,ftell(stream));
//	fclose(stream);
//	}
//	 
//	执行  offset = 5
//	offset =0
//	offset=10
//	 
//	 
//	ftell（取得文件流的读取位置）  
//	定义函数  long ftell(FILE * stream);
//	函数说明  ftell()用来取得文件流目前的读写位置。参数stream为已打开的文件指针。
//	返回值  当调用成功时则返回目前的读写位置，若有错误则返回-1，errno会存放错误代码。
//	 
//	 
//	fwrite（将数据写至文件流）  
//	定义函数  size_t fwrite(const void * ptr,size_t size,size_t nmemb,FILE * stream);
//	函数说明  fwrite()用来将数据写入文件流中。参数stream为已打开的文件指针，参数ptr 指向欲写入的数据地址，总共写入的字符数以参数size*nmemb来决定。Fwrite()会返回实际写入的nmemb数目。
//	返回值  返回实际写入的nmemb数目。 返回数目与实际数目不相符则认为是错误
//	 
//	getchar（由标准输入设备内读进一字符）  
//	定义函数  int getchar(void); equals to getc(stdin) or fgetc(stdin), it's a macro definition
//	函数说明  getchar()用来从标准输入设备中读取一个字符。然后将该字符从unsigned char转换成int后返回。
//	返回值  getchar()会返回读取到的字符，若返回EOF则表示有错误发生。
//	附加说明  getchar()非真正函数，而是getc(stdin)宏定义。
//	 
//	putchar（将指定的字符写到标准输出设备）  
//	定义函数  int putchar (int c); equals to putc(stdout) or fputc(stdout), it's a macro definition
//	函数说明  putchar()用来将参数c字符写到标准输出设备。
//	返回值  putchar()会返回输出成功的字符，即参数c。若返回EOF则代表输出失败。
//	附加说明  putchar()非真正函数，而是putc(c，stdout)宏定义。
//	
//	gets（由标准输入设备内读进一字符串）  
//	定义函数  char * gets(char *s); equals to fgets(char *, int, stdin);
//	函数说明  gets()用来从标准设备读入字符并存到参数s所指的内存空间，直到出现换行字符或读到文件尾为止，最后加上NULL作为字符串结束。
//	返回值  gets()若成功则返回s指针，返回NULL则表示有错误发生。
//	附加说明  由于gets()无法知道字符串s的大小，必须遇到换行字符或文件尾才会结束输入，因此容易造成缓冲溢出的安全性问题。建议使用fgets()取代。
//	
//	puts function is the same to above.
//	 
//	rewind（重设文件流的读写位置为文件开头）  
//	定义函数  void rewind(FILE * stream);
//	函数说明  rewind()用来把文件流的读写位置移至文件开头。参数stream为已打开的文件指针。此函数相当于调用fseek(stream,0,SEEK_SET)。
//	返回值  
//	 
//	ungetc（将指定字符写回文件流中）  
//	定义函数  int ungetc(int c,FILE * stream);
//	函数说明  ungetc()将参数c字符写回参数stream所指定的文件流。这个写回的字符会由下一个读取文件流的函数取得。
//	返回值  成功则返回c 字符，若有错误则返回EOF。
}