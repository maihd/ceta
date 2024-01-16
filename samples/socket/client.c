#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

void error(const char * msg) {
perror(msg);
exit(1);
};
int main(int argc, char * argv[]) {
int portno;
const char * hostname;
if ((argc!=3)) {
fprintf(stderr, "Usage: %s <hostname> <portno>\n", argv[0]);
return 1;
} else {
(portno=atoi(argv[2]));
(hostname=argv[1]);
} ;
int sockfd;
int status;
char buffer[256];
struct addrinfo hints;
struct addrinfo * servinfo;
memset((&hints), 0, sizeof(hints));
(hints.ai_family=AF_INET);
(hints.ai_socktype=SOCK_STREAM);
(status=getaddrinfo(hostname, argv[2], (&hints), (&servinfo)));
if ((status!=0)) {
fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(status));
exit(1);
} ;
struct addrinfo * p;
for ((p=servinfo);(p!=NULL);(p=p->ai_next)) {
(sockfd=socket(AF_INET, SOCK_STREAM, 0));
if ((sockfd<0)) {
perror("Error: opening client socket failed.");
continue;
} ;
if ((connect(sockfd, p->ai_addr, p->ai_addrlen)<0)) {
shutdown(sockfd, 2);
perror("Error: connection failed.");
continue;
} ;
break;
};
if ((p==NULL)) {
fprintf(stderr, "client: failed to connect\n");
exit(1);
} ;
char ip[INET6_ADDRSTRLEN];
inet_ntop(p->ai_family, p->ai_addr, ip, sizeof(ip));
printf("client: connection to %s\n", ip);
freeaddrinfo(servinfo);
int nbytes;
bzero(buffer, sizeof(buffer));
(nbytes=read(sockfd, buffer, sizeof(buffer)));
if ((nbytes<0)) {
error("Error: receive message failed.");
} ;
printf("Received: %s\n", buffer);
printf("Please enter the message: ");
bzero(buffer, sizeof(buffer));
fgets(buffer, sizeof(buffer), stdin);
(nbytes=write(sockfd, buffer, strlen(buffer)));
if ((nbytes<0)) {
error("Error: sending message to server failed.");
} ;
return 0;
};
