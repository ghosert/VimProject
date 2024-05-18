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
	
//	fgetc�����ļ��ж�ȡһ���ַ���  
//	���庯��  int fgetc(FILE * stream);
//	����˵��  fgetc()�Ӳ���stream��ָ���ļ��ж�ȡһ���ַ����������ļ�β��������ʱ�㷵��EOF��
//	same to getc() getc() is a macro definition
//	
//	fputc����һָ���ַ�д���ļ����У�  
//	���庯��  int fputc(int c,FILE * stream);
//	����ֵ  fputc()�᷵��д��ɹ����ַ���������c��������EOF�����д��ʧ�ܡ�
//	same to putc() putc() is a macro definition
//	
//	fgets�����ļ��ж�ȡһ���ַ�����  
//	���庯��  char * fgets(char * s,int size,FILE * stream);
//	����˵��  fgets()�����Ӳ���stream��ָ���ļ��ڶ����ַ����浽����s��ָ���ڴ�ռ䣬ֱ�����ֻ����ַ��������ļ�β�����Ѷ���size-1���ַ�Ϊֹ���������NULL��Ϊ�ַ���������
//	 
//	fputs����һָ�����ַ���д���ļ��ڣ�  
//	���庯��  int fputs(const char * s,FILE * stream);
//	����˵��  fputs()����������s��ָ���ַ���д�뵽����stream��ָ���ļ��ڡ�
//	����ֵ  ���ɹ��򷵻�д�����ַ�����������EOF���ʾ�д�������
//	
//	fileno�������ļ�����ʹ�õ��ļ������ʣ�  
//	���庯��  int fileno(FILE * stream);
//	����˵��  fileno()����ȡ�ò���streamָ�����ļ�����ʹ�õ��ļ������ʡ�return fd;
//	 
//	fopen�����ļ���  
//	���庯��  FILE * fopen(const char * path,const char * mode);
//	mode�����м�����̬�ַ���:
//	r ��ֻ���ļ������ļ�������ڡ�
//	r+ �򿪿ɶ�д���ļ������ļ�������ڡ�
//	w ��ֻд�ļ������ļ��������ļ�������Ϊ0�������ļ����ݻ���ʧ�����ļ��������������ļ���
//	w+ �򿪿ɶ�д�ļ������ļ��������ļ�������Ϊ�㣬�����ļ����ݻ���ʧ�����ļ��������������ļ���
//	a �Ը��ӵķ�ʽ��ֻд�ļ������ļ������ڣ���Ὠ�����ļ�������ļ����ڣ�д������ݻᱻ�ӵ��ļ�β�����ļ�ԭ�ȵ����ݻᱻ������
//	a+ �Ը��ӷ�ʽ�򿪿ɶ�д���ļ������ļ������ڣ���Ὠ�����ļ�������ļ����ڣ�д������ݻᱻ�ӵ��ļ�β�󣬼��ļ�ԭ�ȵ����ݻᱻ������
//	��������̬�ַ����������ټ�һ��b�ַ�����rb��w+b��ab������ϣ�����b �ַ��������ߺ�����򿪵��ļ�Ϊ�������ļ������Ǵ������ļ���������POSIXϵͳ������Linux������Ը��ַ�����fopen()�����������ļ������S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH(0666)Ȩ�ޣ����ļ�Ȩ��Ҳ��ο�umaskֵ��
//	����ֵ  �ļ�˳���򿪺�ָ��������ļ�ָ��ͻᱻ���ء������ļ���ʧ���򷵻�NULL�����Ѵ���������errno �С�
//	 
//	fread�����ļ�����ȡ���ݣ�  
//	���庯��  size_t fread(void * ptr,size_t size,size_t nmemb,FILE * stream);
//	����˵��  fread()�������ļ����ж�ȡ���ݡ�����streamΪ�Ѵ򿪵��ļ�ָ�룬����ptr ָ������Ŷ�ȡ���������ݿռ�
//	��ȡ���ַ����Բ���size*nmemb��������Fread()�᷵��ʵ�ʶ�ȡ����nmemb��Ŀ�������ֵ�Ȳ���nmemb ����С
//	�������ܶ������ļ�β���д���������ʱ������feof()��ferror()����������ʲô�����
//	����ֵ  ����ʵ�ʶ�ȡ����nmemb��Ŀ��
//	 
//	fseek���ƶ��ļ����Ķ�дλ�ã�  
//	���庯��  int fseek(FILE * stream,long offset,int whence);
//	����˵��  fseek()�����ƶ��ļ����Ķ�дλ�á�����streamΪ�Ѵ򿪵��ļ�ָ�룬����offsetΪ���ݲ���whence���ƶ���дλ�õ�λ������
//	����ֵ  �����óɹ�ʱ�򷵻�0�����д����򷵻�-1��errno���Ŵ�����롣
//	����˵��  fseek()����lseek()�᷵�ض�дλ�ã���˱���ʹ��ftell()��ȡ��Ŀǰ��д��λ�á�
//	����  #include<stdio.h>
//	main()
//	{
//	FILE * stream;
//	long offset;
//	fpos_t pos;
//	stream=fopen(��/etc/passwd��,��r��);
//	fseek(stream,5,SEEK_SET);
//	printf(��offset=%d\n��,ftell(stream)); // ftell return -f if error
//	rewind(stream);
//	fgetpos(stream,&pos);
//	printf(��offset=%d\n��,pos);
//	pos=10;
//	fsetpos(stream,&pos);
//	printf(��offset = %d\n��,ftell(stream));
//	fclose(stream);
//	}
//	 
//	ִ��  offset = 5
//	offset =0
//	offset=10
//	 
//	 
//	ftell��ȡ���ļ����Ķ�ȡλ�ã�  
//	���庯��  long ftell(FILE * stream);
//	����˵��  ftell()����ȡ���ļ���Ŀǰ�Ķ�дλ�á�����streamΪ�Ѵ򿪵��ļ�ָ�롣
//	����ֵ  �����óɹ�ʱ�򷵻�Ŀǰ�Ķ�дλ�ã����д����򷵻�-1��errno���Ŵ�����롣
//	 
//	 
//	fwrite��������д���ļ�����  
//	���庯��  size_t fwrite(const void * ptr,size_t size,size_t nmemb,FILE * stream);
//	����˵��  fwrite()����������д���ļ����С�����streamΪ�Ѵ򿪵��ļ�ָ�룬����ptr ָ����д������ݵ�ַ���ܹ�д����ַ����Բ���size*nmemb��������Fwrite()�᷵��ʵ��д���nmemb��Ŀ��
//	����ֵ  ����ʵ��д���nmemb��Ŀ�� ������Ŀ��ʵ����Ŀ���������Ϊ�Ǵ���
//	 
//	getchar���ɱ�׼�����豸�ڶ���һ�ַ���  
//	���庯��  int getchar(void); equals to getc(stdin) or fgetc(stdin), it's a macro definition
//	����˵��  getchar()�����ӱ�׼�����豸�ж�ȡһ���ַ���Ȼ�󽫸��ַ���unsigned charת����int�󷵻ء�
//	����ֵ  getchar()�᷵�ض�ȡ�����ַ���������EOF���ʾ�д�������
//	����˵��  getchar()����������������getc(stdin)�궨�塣
//	 
//	putchar����ָ�����ַ�д����׼����豸��  
//	���庯��  int putchar (int c); equals to putc(stdout) or fputc(stdout), it's a macro definition
//	����˵��  putchar()����������c�ַ�д����׼����豸��
//	����ֵ  putchar()�᷵������ɹ����ַ���������c��������EOF��������ʧ�ܡ�
//	����˵��  putchar()����������������putc(c��stdout)�궨�塣
//	
//	gets���ɱ�׼�����豸�ڶ���һ�ַ�����  
//	���庯��  char * gets(char *s); equals to fgets(char *, int, stdin);
//	����˵��  gets()�����ӱ�׼�豸�����ַ����浽����s��ָ���ڴ�ռ䣬ֱ�����ֻ����ַ�������ļ�βΪֹ��������NULL��Ϊ�ַ���������
//	����ֵ  gets()���ɹ��򷵻�sָ�룬����NULL���ʾ�д�������
//	����˵��  ����gets()�޷�֪���ַ���s�Ĵ�С���������������ַ����ļ�β�Ż�������룬���������ɻ�������İ�ȫ�����⡣����ʹ��fgets()ȡ����
//	
//	puts function is the same to above.
//	 
//	rewind�������ļ����Ķ�дλ��Ϊ�ļ���ͷ��  
//	���庯��  void rewind(FILE * stream);
//	����˵��  rewind()�������ļ����Ķ�дλ�������ļ���ͷ������streamΪ�Ѵ򿪵��ļ�ָ�롣�˺����൱�ڵ���fseek(stream,0,SEEK_SET)��
//	����ֵ  
//	 
//	ungetc����ָ���ַ�д���ļ����У�  
//	���庯��  int ungetc(int c,FILE * stream);
//	����˵��  ungetc()������c�ַ�д�ز���stream��ָ�����ļ��������д�ص��ַ�������һ����ȡ�ļ����ĺ���ȡ�á�
//	����ֵ  �ɹ��򷵻�c �ַ������д����򷵻�EOF��
}