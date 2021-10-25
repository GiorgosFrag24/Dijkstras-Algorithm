#include<stdio.h>
#include<string.h>
#include<math.h>
#include<stdbool.h>
#include<stdlib.h>
#define INF 1000000000

int source[2]; 			// coordinates for starting node
int ***min_distance; 	// global array holding the minimum distance travelled for each node and each state of that node(holding the packet or not)



typedef struct node { 	//node object (needs also a prev pointer : to be implemented)
    char type;    		//S,E,W,X,.
	int coor[2];		//coordinates
	int packet ;		//1 is with, 0 is without
    struct node *next,*prev;
	int distance;
} node_t;


node_t *head0 = NULL;
node_t *head1 = NULL; //distance 1
node_t *head2 = NULL; //distance 2
node_t *head3 = NULL; //distance 3


void push(node_t ** head,node_t *prev,int coord[2],char type,int packet,int cost) {
    node_t * new_node;
    new_node = malloc(sizeof(node_t));
    new_node->coor[0] = coord[0];
    new_node->coor[1] = coord[1];
	new_node->packet = packet;
	new_node->type = type;
	new_node->distance = cost;
    new_node->next = *head;
    new_node->prev = prev;
    *head = new_node;
}



//αν έχουμε φτάσει σε ένα τετράγωνο με ένα συγκεκριμένο κόστος μην το ξαναεπισκεφτείς


void print_path(node_t *node,int final_cost){
    node_t *current = node;
    node_t *previous = node->prev;
    char path[final_cost] ;
    int i=-1;
    while(previous!=NULL){
        i++;
        switch(previous->coor[0]-current->coor[0]){
			case 0:
				switch(previous->coor[1]-current->coor[1]){
					case 1:
						if(current->packet!=previous->packet){

                            path[i]='W';
                            path[i+1]='L';

							i++;
							current = current->prev;
							previous = previous->prev;
							break;
						}
						else{
							path[i]='L';
							current = current->prev;
							previous = previous->prev;
							break;
						}
					case -1:
						if(current->packet!=previous->packet){

                            path[i]='W';
                            path[i+1]='R';

							i++;
							current = current->prev;
							previous = previous->prev;
							break;
						}
						else{
							path[i]='R';
							current = current->prev;
							previous = previous->prev;
							break;
						}
                }
			break;
			case 1:
				if(current->packet!=previous->packet){
                            path[i]='W';
                            path[i+1]='U';

                    i++;
                    current = current->prev;
                    previous = previous->prev;
                    break;
                }
                else{

					path[i]='U';
					current = current->prev;
					previous = previous->prev;
					break;
                }
			case (-1):
				if(current->packet!=previous->packet){

                            path[i]='W';
                            path[i+1]='D';

                    i++;
                    current = current->prev;
                    previous = previous->prev;
                    break;
                }
                else{
					path[i]='D';
					current = current->prev;
					previous = previous->prev;
					break;
                }
        }
    }
    int j;
    for(j=i;j>=0;j--){
        printf("%c",path[j]);
    }
}




void get_frontier(node_t *node,char **map,int N,int M,int ***min_distance){ // current cost is the upper limit

	int nb[4][2] = {
    {node->coor[0]-1,node->coor[1]},	//up
	{node->coor[0],node->coor[1]+1}, 	//right
	{node->coor[0]+1,node->coor[1]}, 	//down
	{node->coor[0],node->coor[1]-1}};	//left

	int i,x,y,coor[2];
	char type;
	int tempCost = node->distance;		//node distance tells us the sofar travelled distance to reach that node
	bool in_bounds ;
	for ( i = 0;i<4;i++){
         x = nb[i][0];
         y = nb[i][1];
         in_bounds = x>=0 && x<N && y>=0 && y<M;
        if(in_bounds){
			type = map[x][y];
			coor[0]=x;
			coor[1]=y;
			tempCost = node->distance + 1 + node->packet;	// potential cost of move is 1 plus 1 if we have the packet with us
            if (type == 'X' ){								//if obstacle ignore
						continue;
					}
            else if (tempCost < min_distance[x][y][node->packet] ){ //if the potential cost to reach a node is smaller than the up
																	//to now cost to reach that node with the same state
					min_distance[x][y][node->packet] = tempCost;	//assign new minimum distance

					if (type == 'W' ){								//if W, we either move without an action or we move and act
						if(node->packet == 1){
                                push(&head3,node,coor,type,0,tempCost+1);
                                push(&head2,node,coor,type,1,tempCost);

						}
						else{   //not packet and pickup or packet and not pickup
                                push(&head2,node,coor,type,1,tempCost+1);
                                push(&head1,node,coor,type,0,tempCost);
						}
					}
					else if (type == '.' ){
                        if(node->packet==0){
                                push(&head1,node,coor,type,0,tempCost);
                        }
                        else{
                                push(&head2,node,coor,type,1,tempCost);
                        }

					}
					else if (type == 'S' ){
                        if(node->packet==0){
                                push(&head1,node,coor,type,0,tempCost);
                        }
                        else{
                                push(&head2,node,coor,type,1,tempCost);
                        }

					}
					else if(type=='E' ){
                        if(node->packet==0){
                            push(&head1,node,coor,type,0,tempCost);
                        }
                        else{
                            push(&head2,node,coor,type,1,tempCost);
                        }

                    }
			}
        }
	}
}



int get_N(char *filename)
{
  FILE *fp;

  fp = fopen(filename,"r");

  int ch=0;
  int N=0;
  if (fp == NULL){
        printf("%s\n",filename);
        return 0;
  }

  N++;
  while(!feof(fp))
{
  ch = fgetc(fp);

  if(ch == '\n')
  {
    N++;
  }
}
  fclose(fp);
  return N;
}

int get_M(char *filename)
{

  FILE *fp = fopen(filename,"r");
  int ch=0;
  int M = 0;
  if (fp == NULL){
    return 0;
  }


  while(!feof(fp))
{
  ch = fgetc(fp);
  M++;
  if(ch == '\n')
  {
    break;
  }
}
  fclose(fp);
  return M;
}

int main(int argc, char **argv){

    int N = get_N(argv[1]);
	int M = get_M(argv[1]);
	int i,j;
    FILE *fp = fopen(argv[1],"r");
    char c;
    char **map = (char **)malloc(N * sizeof(char *));
    for (i=0; i<N; i++)
         map[i] = (char *)malloc(M * sizeof(char));
    for ( i = 0; i < N; i++){
        for ( j = 0; j < M; j++){
                c = fgetc(fp);
				if (c=='S'){
					source[0]=i;
					source[1]=j;
				}
                map[i][j] = c;
        }
    }
    fclose(fp);

	min_distance = (int***)malloc(N*sizeof(int **));

	 for (i=0; i<N; i++){min_distance[i] = (int **)malloc(M * sizeof(int *));}

	 for (i=0; i<N; i++){
		for (j=0; j<M; j++){
			min_distance[i][j] = (int *)malloc(2 * sizeof(int ));
		}
	 }
	 for (i=0; i<N; i++){
		for (j=0; j<M; j++){
			min_distance[i][j][0] = INF;
			min_distance[i][j][1] = INF;
		}
	 }

	int found = 0;
	push(&head0,NULL,source,'S',1,0); // initialize d0 with source coor (current and prev) , its type , carrying , zero cost
	min_distance[source[0]][source[1]][1]=INF;
	min_distance[source[0]][source[1]][0]=INF;
	while (found == 0){
        while(head0 != NULL){
                if(head0->type == 'E' && head0->packet == 1){
					printf("%d ",head0->distance    );
                    print_path(head0,head0->distance);
					found = 1;
					head0 = NULL;
					}
					else{
                        get_frontier(head0,map,N,M,min_distance);
                        head0 = head0->next;
                    }
                }
                head0 = head1;
				head1 = head2;
				head2 = head3;
				head3 = NULL;
	}
	return 0;
}
