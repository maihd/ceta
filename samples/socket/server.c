#define kDirSize 8192 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

typedef struct sockaddr_in sockaddr_in_t;
void error(const char * msg);
int main(int argc, char * argv[]) {
int sockfd;
int newsockfd;
int portno;
int clilen;
int n;
char buffer[256];
sockaddr_in_t seraddr;
sockaddr_in_t cliaddr;
if ((argc<2)) {
fprintf(stderr, "Error: no port provided.\n");
exit(1);
return 1;
} else {
(portno=atoi(argv[1]));
} ;
(sockfd=socket(AF_INET, SOCK_STREAM, 0));
if ((sockfd<0)) {
error("Error: opening socking failed.");
} ;
printf("Server ip: %s\n", inet_addr("127.0.0.1"));
memset((&seraddr), 0, sizeof(seraddr));
(seraddr.sin_family=AF_INET);
(seraddr.sin_port=htons(portno));
(seraddr.sin_addr.s_addr=inet_addr("127.0.0.1"));
if ((bind(sockfd, ((struct sockaddr *)(&seraddr)), sizeof(seraddr))<0)) {
error("Error: binding failed.");
} ;
listen(sockfd, 5);
(clilen=sizeof(cliaddr));
(newsockfd=accept(sockfd, ((struct sockaddr *)(&cliaddr)), (&clilen)));
if ((newsockfd<0)) {
error("Error: accept failed.");
} ;
bzero(buffer, sizeof(buffer));
(n=read(newsockfd, buffer, (sizeof(buffer)-1)));
if ((n<0)) {
error("Error: reading from socket failed.");
} ;
printf("The message from client: %d\n", buffer);
(n=write(newsockfd, "I got your message", sizeof("I got your message")));
if ((n<0)) {
error("Error: writing to socket failed.");
} ;
getchar();
return 0;
};
void error(const char * msg) {
perror(msg);
exit(1);
};
