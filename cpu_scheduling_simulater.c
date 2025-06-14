#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define TIME_QUANTUM 3

typedef struct process {
    int pid;
    int priority;
    int arrivalTime;
    int CPUburst;
    int IOburst;
    int waitingTime;
    int turnaroundTime;
}process;

process* job_queue[30];
process* ready_queue[30];
process* waiting_queue[30];
process* terminated[30];

int pos_j = 0;
int pos_r = 0;
int pos_w = 0;
int pos_t = 0;

process* running = NULL;
int time_consumed = 0;

int start_time = 0;
int idle_time = 0;
int end_time = 0;

int remember = 0;
int n = 0;

process* create_process(int pid, int priority, int arrivalTime, int CPUburst, int IOburst);
void print_processes();

void init_job_queue();
int get_job_queue(int pid);
void insert_job_queue(process* p);
void clear_job_queue();

void init_ready_queue();
int get_ready_queue(int pid);
void insert_ready_queue(process* p);
process* remove_ready_queue(process* p);
void clear_ready_queue();

void init_waiting_queue();
int get_waiting_queue(int pid);
void insert_waiting_queue(process* p);
process* remove_waiting_queue(process* p);
void clear_waiting_queue();

void init_terminated();
int get_terminated(int pid);
void insert_terminated(process* p);
void clear_terminated();

process* find_important();
process* find_shortest();

process* FCFS();
process* preemptive_SJF();
process* nonpreemptive_SJF();
process* preemptive_priority();
process* nonpreemptive_priority();
process* RR();

void start_simulate(int algorithm);
void simulate(int algorithm, int cnt);

void evaluate();

void main(int argc, char **argv){
	int process_num = atoi(argv[1]);
    int IO_num = atoi(argv[2]);
    
    int i;
    
    init_job_queue();
    init_ready_queue();
    init_waiting_queue();
    init_terminated();
    
    srand(time(NULL));
    
    for(i=0;i<process_num;i++){
    	create_process(i+1, rand() % process_num + 1, rand() % (process_num + 10), rand() % 11 + 5, 0);
	}

	for(i=0;i<IO_num;i++){
		int index = rand() % process_num;
		if(job_queue[index]->IOburst == 0){
			int randIO = rand() % 10 + 1;
			job_queue[index]->IOburst = randIO;
		}
		else{
			i--;
		}
	}
	
	printf("total process number: %d\n", process_num);
	print_processes();
	
	for(i=0;i<6;i++){
		start_simulate(i);
	}
	
	clear_job_queue();
}

process* create_process(int pid, int priority, int arrivalTime, int CPUburst, int IOburst){
	process *newProcess = (process*)malloc(sizeof(process));
	
	newProcess->pid = pid;
    newProcess->priority = priority;
    newProcess->arrivalTime = arrivalTime;
    newProcess->CPUburst = CPUburst;
    newProcess->IOburst = IOburst;
    newProcess->waitingTime = 0;
    newProcess->turnaroundTime = 0;
	
	insert_job_queue(newProcess);
	
	return newProcess;
}

void print_processes(){
	puts("pid    priority    arrival_time    CPU burst    IO burst");
	puts("========================================================");
    for(int i = 0; i < pos_j; i++) {
        printf("%3d    %8d    %12d    %9d    %8d\n", job_queue[i]->pid, job_queue[i]->priority, job_queue[i]->arrivalTime, job_queue[i]->CPUburst, job_queue[i]->IOburst);   
    }
    puts("========================================================\n");
}

void init_job_queue(){
	pos_j = 0;
	
	for(int i=0;i<30;i++){
		job_queue[i] = NULL;
	}
}

int get_job_queue(int pid){
	int res = -1;
	int i;
	for(i=0;i<pos_j;i++){
		int tmp = job_queue[i]->pid;
		if(tmp == pid){
			return i;
		}
	}
	return res;	
}

void insert_job_queue(process* p){
	if(pos_j<30){
        int tmp = get_job_queue(p->pid);
        if (tmp != -1){
            return;  
    	}
        job_queue[pos_j++] = p;
    }
    else{
        return;
    }
}

void clear_job_queue(){
	pos_j = 0;
	
	for(int i=0;i<30;i++){
		free(job_queue[i]);
		job_queue[i] = NULL;
	}
}

void init_ready_queue(){
	pos_r = 0;
	
	for(int i=0;i<30;i++){
		ready_queue[i] = NULL;
	}
}

int get_ready_queue(int pid){
	int res = -1;
	int i;
	for(i=0;i<pos_r;i++){
		int tmp = ready_queue[i]->pid;
		if(tmp == pid){
			return i;
		}
	}
	return res;	
}

void insert_ready_queue(process* p){
	if(pos_r<30){
        int tmp = get_ready_queue(p->pid);
        if (tmp != -1){
            return;  
    	}
        ready_queue[pos_r++] = p;
    }
    else{
        return;
    }
}

process* remove_ready_queue(process* p){
	if(pos_r>0){
        int tmp = get_ready_queue(p->pid);
        if (tmp == -1){
            return NULL;    
        }
		else{
            process* remove = ready_queue[tmp];
            
            for(int i = tmp; i < pos_r-1; i++)
                ready_queue[i] = ready_queue[i+1];   
                
            ready_queue[--pos_r] = NULL;
            
            return remove;
        }
    }
	else{
        return NULL;
    }
}

void clear_ready_queue(){
	pos_r = 0;
	
	for(int i=0;i<30;i++){
		free(ready_queue[i]);
		ready_queue[i] = NULL;
	}
}

void init_waiting_queue(){
	pos_w = 0;
	
	for(int i=0;i<30;i++){
		waiting_queue[i] = NULL;
	}
}

int get_waiting_queue(int pid){
	int res = -1;
	int i;
	for(i=0;i<pos_w;i++){
		int tmp = waiting_queue[i]->pid;
		if(tmp == pid){
			return i;
		}
	}
	return res;	
}

void insert_waiting_queue(process* p){
	if(pos_w<30){
        int tmp = get_waiting_queue(p->pid);
        if (tmp != -1){
            return;  
    	}
        waiting_queue[pos_w++] = p;
    }
    else{
        return;
    }
}

process* remove_waiting_queue(process* p){
	if(pos_w>0){
        int tmp = get_waiting_queue(p->pid);
        if (tmp == -1){
            return NULL;
        }
		else{
            process* remove = waiting_queue[tmp];
            
            for(int i = tmp; i < pos_w - 1; i++)
                waiting_queue[i] = waiting_queue[i+1];   
                
            waiting_queue[--pos_w] = NULL;
            
            return remove;
        }
    }
	else{
        return NULL;
    }
}

void clear_waiting_queue(){
	pos_w = 0;
	
	for(int i=0;i<30;i++){
		free(waiting_queue[i]);
		waiting_queue[i] = NULL;
	}
}

void init_terminated(){
	pos_t = 0;
	
	for(int i=0;i<30;i++){
		terminated[i] = NULL;
	}
}

int get_terminated(int pid){
	int res = -1;
	int i;
	for(i=0;i<pos_t;i++){
		int tmp = terminated[i]->pid;
		if(tmp == pid){
			return i;
		}
	}
	return res;	
}

void insert_terminated(process* p){
	if(pos_t<30){
        int tmp = get_terminated(p->pid);
        if (tmp != -1){
            return;  
    	}
        terminated[pos_t++] = p;
    }
    else{
        return;
    }
}

void clear_terminated(){
	pos_t = 0;
	
	for(int i=0;i<30;i++){
		free(terminated[i]);
		terminated[i] = NULL;
	}
}

process* find_important(){
	process* important = ready_queue[0];
	
	int i;
    for(i = 0; i < pos_r; i++) {
    	if (ready_queue[i]->priority <= important->priority) { 
            if(ready_queue[i]->priority == important->priority) {
                if (ready_queue[i]->arrivalTime < important->arrivalTime)
					important = ready_queue[i];
            }
			else {
                important = ready_queue[i];
            }
    	}
    }
    
    return important;
}

process* find_shortest(){
	process* shortest = ready_queue[0];
	
	int i;
    for(i = 0; i < pos_r; i++) {
    	if (ready_queue[i]->CPUburst <= shortest->CPUburst) { 
            if(ready_queue[i]->CPUburst == shortest->CPUburst) {
                if (ready_queue[i]->arrivalTime < shortest->arrivalTime)
					shortest = ready_queue[i];
            }
			else {
                shortest = ready_queue[i];
            }
    	}
    }
    
    return shortest;
}

process* FCFS(){
	process* earliest = ready_queue[0];
	
	if(earliest != NULL){
		if(running != NULL){
			return running;
		}
		else{
			return remove_ready_queue(earliest);
		}
	}
	
	else{
		return running;
	}
}

process* preemptive_SJF(){
	process* shortest = find_shortest();
	
	if(shortest != NULL){
		if(running != NULL){
			if(running->CPUburst > shortest->CPUburst){
				insert_ready_queue(running);
				return remove_ready_queue(shortest);
			}
			else if(running->CPUburst == shortest->CPUburst){
				if(running->arrivalTime > shortest->arrivalTime){
					insert_ready_queue(running);
					return remove_ready_queue(shortest);
				}
				else{
					return running;
				}
			}
			else{
				return running;
			}
		}
		else{
			return remove_ready_queue(shortest);
		}
	}
	
	else{
		return running;
	}
}

process* nonpreemptive_SJF(){
	process* shortest = find_shortest();
	
	if(shortest != NULL){
		if(running != NULL){
			return running;
		}
		else{
			return remove_ready_queue(shortest);
		}
	}
	
	else{
		return running;
	}
}

process* preemptive_priority(){
	process* important = find_important();
	
	if(important != NULL){
		if(running != NULL){
			if(running->priority > important->priority){
				insert_ready_queue(running);
				return remove_ready_queue(important);
			}
			else if(running->priority == important->priority){
				if(running->arrivalTime > important->arrivalTime){
					insert_ready_queue(running);
					return remove_ready_queue(important);
				}
				else{
					return running;
				}
			}
			else{
				return running;
			}
		}
		else{
			return remove_ready_queue(important);
		}
	}
	
	else{
		return running;
	}
}

process* nonpreemptive_priority(){
	process* important = find_important();
	
	if(important != NULL){
		if(running != NULL){
			return running;
		}
		else{
			return remove_ready_queue(important);
		}
	}
	
	else{
		return running;
	}
}

process* RR(){
	process* earliest = ready_queue[0];
	
	if(earliest != NULL){
		if(running != NULL){
			if(time_consumed >= TIME_QUANTUM){
				insert_ready_queue(running);
				return remove_ready_queue(earliest);
			}
			else{
				return running;
			}
		}
		else{
			return remove_ready_queue(earliest);
		}
	}
	
	else{
		return running;
	}
}

void start_simulate(int algorithm){
	time_consumed = 0;
	start_time = 0;
	idle_time = 0;
	end_time = 0;
	remember = 0;
	
	running = NULL;
	
	int process_num = pos_j;
	int cnt = 0;
	
	if(pos_j <= 0){
		return;
	}
	
	start_time = job_queue[0]->arrivalTime;
	for(int i = 0; i < pos_j; i++){
		if(job_queue[i]->arrivalTime < start_time){
			start_time = job_queue[i]->arrivalTime;
		}
	}
	idle_time = 0;
	
	switch(algorithm){
		case 0: printf("<FCFS>\n"); break;
		case 1: printf("<Preemptive SJF>\n"); break;
		case 2: printf("<Nonpreemptive SJF>\n"); break;
		case 3: printf("<Preemptive Priority>\n"); break;
		case 4: printf("<Nonpreemptive Priority>\n"); break;
		case 5: printf("<RR>\n"); break;
		default: return;
	}
	
	puts("--------------------------------------------------------");
	printf("|");
	
	while(pos_t < pos_j){
		simulate(algorithm, cnt);
		cnt++;
		if(pos_t == pos_j){
			break;
		}
	}
	
	for(int i=0;i<time_consumed/3;i++){
		printf(" ");
	}
		printf("%d", remember);
	for(int i=0;i<time_consumed/3;i++){
		printf(" ");
	}	
	printf("|");
	
	end_time = cnt-1;
	
	printf("\n");
	puts("--------------------------------------------------------");
	evaluate();
	
	clear_ready_queue();
	clear_waiting_queue();
	clear_terminated();
}

void simulate(int algorithm, int cnt) { 
	process* tmp = NULL;
	int process_num = pos_j;
	int i;
	
	for(i = 0; i < pos_j; i++){
		if(job_queue[i]->arrivalTime == cnt){
			tmp = (process*)malloc(sizeof(process));
			
			tmp->pid = job_queue[i]->pid;
    		tmp->priority = job_queue[i]->priority;
    		tmp->arrivalTime = job_queue[i]->arrivalTime;
    		tmp->CPUburst = job_queue[i]->CPUburst;
    		tmp->IOburst = job_queue[i]->IOburst;
    		tmp->waitingTime = 0;
    		tmp->turnaroundTime = 0;
    		
			insert_ready_queue(tmp);
		}
	}
	
	tmp = NULL;
	process* prev = running;
	
	switch(algorithm){
		case 0: running = FCFS(); break;
		case 1: running = preemptive_SJF(); break;
		case 2: running = nonpreemptive_SJF(); break;
		case 3: running = preemptive_priority(); break;
		case 4: running = nonpreemptive_priority(); break;
		case 5: running = RR(); break;
		default: return; 
	}
	
	if(prev != running){
		if(prev != NULL){
			remember = prev->pid;
		}
		
		if(remember > 0){
			for(i=0;i<time_consumed/3;i++){
				printf(" ");
			}
			printf("%d", remember);
			for(i=0;i<time_consumed/3;i++){
				printf(" ");
		 	}	
			printf("|");
		}
		
		if(prev == NULL){
			for(i=0;i<n;i++){
				printf(" |");
			}
			n = 0;
		}
		
		time_consumed = 0;
	}
	
	
	
    for(i = 0; i < pos_r; i++){
        if(ready_queue[i] != NULL){
        	ready_queue[i]->waitingTime++;
        	ready_queue[i]->turnaroundTime++;
    	}
    }
	
    for(i = 0; i < pos_w; i++){ 
		if(waiting_queue[i] != NULL){
			waiting_queue[i]->turnaroundTime++;
			waiting_queue[i]->IOburst--;
			
			if(waiting_queue[i]->IOburst <= 0 ){
				tmp = remove_waiting_queue(waiting_queue[i--]);
				insert_ready_queue(tmp);
			}
		}
	}
	
    if(running != NULL){
        running->CPUburst--;
        running->turnaroundTime++;
        time_consumed++;
        
        if(running->CPUburst <= 0){
			insert_terminated(running);
			remember = running->pid;
			running = NULL;
		}
		else{
			if(running->IOburst > 0){
				insert_waiting_queue(running);
				remember = running->pid;
				running = NULL;	
			}
		}
    }
	else{
		n++;
    	idle_time++;
	}
}

void evaluate(){
	int i;
	double cpu_utilization = 0;
	int waiting_sum = 0, turnaround_sum = 0;
	
	process* p = NULL;
	
	for(i=0;i<pos_t;i++){
		waiting_sum += terminated[i]->waitingTime;
		turnaround_sum += terminated[i]->turnaroundTime;
	}
	
	cpu_utilization = (double)(end_time - idle_time) / (end_time - start_time);
	
	puts("========================================================");
	printf("start time: %d, end time: %d\n", start_time, end_time);
	printf("cpu utilization: %.2lf%%\n", cpu_utilization * 100);
	printf("Average waiting time: %.2lf\n", (double)waiting_sum / pos_j);
	printf("Average turnaround time: %.2lf\n", (double)turnaround_sum / pos_j);
	puts("========================================================");
}

