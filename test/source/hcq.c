
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "hcq.h"
#define INPUT_BUFFER_SIZE 256

/*
 * Return a pointer to the struct student with name stu_name
 * or NULL if no student with this name exists in the stu_list
 */
Student *find_student(Student *stu_list, char *student_name) {
  Student *curr = stu_list;
  while (curr != NULL) {
    if(!strcmp(curr -> name, student_name)){
      return curr;
    }

    curr = curr -> next_overall;
  }
    return NULL; // not found
}

void *malloc_c(int size, char *err){
  void *ptr = malloc(size);
  if(ptr == NULL){
    perror(err);
    exit(1);
  }

  return ptr;
}

/*   Return a pointer to the ta with name ta_name or NULL
 *   if no such TA exists in ta_list.
 */
Ta *find_ta(Ta *ta_list, char *ta_name) {
  Ta *curr = ta_list;
  while (curr != NULL) {
    if(!strcmp(curr -> name, ta_name)){
      return curr;
    }

    curr = curr -> next;
  }
    return NULL; // not found
}



/*  Return a pointer to the course with this code in the course list
 *  or NULL if there is no course in the list with this code.
 */
Course *find_course(Course *courses, int num_courses, char *course_code) {
  int i;
  for (i = 0; i < num_courses; i++) {
    if(!strcmp(courses[i].code, course_code)){
      return &(courses[i]);
    }
  }

  return NULL; // not found
}



/* Add a student to the queue with student_name and a question about course_code.
 * if a student with this name already has a question in the queue (for any
   course), return 1 and do not create the student.
 * If course_code does not exist in the list, return 2 and do not create
 * the student struct.
 * For the purposes of this assignment, don't check anything about the
 * uniqueness of the name.
 */
int add_student(Student **stu_list_ptr, char *student_name, char *course_code,
    Course *course_array, int num_courses) {


    // course to add to
    Course *course = find_course(course_array, num_courses, course_code);



    Student *new = malloc_c(sizeof(Student), "malloc  new student");

    // Set the attributes of the new student
    new -> name = malloc_c(sizeof(char) * (strlen(student_name) + 1), "aaaa ");
    strcpy(new -> name, student_name);
    new -> arrival_time = malloc_c(sizeof(time_t), "mallocaa");
    new -> next_course = NULL;
    new -> next_overall = NULL;
    new -> course = course;
    
    *(new -> arrival_time) = time(NULL);
    if(*stu_list_ptr == NULL){
      *stu_list_ptr = new;
    }else{
        Student *curr = *stu_list_ptr; //get the first student in line
        while(curr -> next_overall != NULL){
          if(!strcmp(curr -> name, student_name)){
            return 1;
          }
          curr = curr -> next_overall;
        }

        if(!strcmp(curr -> name, student_name)){ //check the name of last
          return 1;
        }

        curr -> next_overall = new;
    }

    if(course == NULL){
      return 2;
    }


    // Link the student to their course
    if(course -> head == NULL){
      course -> head = new;
      course -> tail = new;
    }else{
      course -> tail -> next_course = new;
      course -> tail = new;
  }

    return 0;
}

/*Remove a student from the queue and make sure all the links are intact
*/
void dequeue_student(Student **stu_list_ptr, Student *student){

  Student *curr = *stu_list_ptr;
  if(curr == student){ // handle where first is desired student
    *stu_list_ptr = curr -> next_overall;
  }else{ //search for student
    while(curr != NULL){
      if(curr -> next_course == student){ // course chain
        curr -> next_course = student -> next_course;
      }

      if(curr -> next_overall == student){ // full chain
        curr -> next_overall = student -> next_overall;
      }
      curr = curr -> next_overall;
    }
  }

  // fix the student links in the course
  Course *course = student -> course;
  if(course -> head == student){
    course -> head = student -> next_course;
  }
  if(course -> tail == student){
    course -> tail = NULL;
  }

}


/* Student student_name has given up waiting and left the help centre
 * before being called by a Ta. Record the appropriate statistics, remove
 * the student from the queues and clean up any no-longer-needed memory.
 *
 * If there is no student by this name in the stu_list, return 1.
 */
int give_up_waiting(Student **stu_list_ptr, char *student_name) {
    Student *student = find_student(*stu_list_ptr, student_name);
    if(student == NULL){
      return 1;
    }

    dequeue_student(stu_list_ptr, student); // removes and restores links

    //Update stats
    Course *course = student -> course;
    (course -> bailed)++;
    course -> wait_time += time(NULL) - *(student -> arrival_time);

    free(student -> arrival_time);
    free(student -> name);
    free(student);

    return 0;
}

/* Create and prepend Ta with ta_name to the head of ta_list.
 * For the purposes of this assignment, assume that ta_name is unique
 * to the help centre and don't check it.
 */
void add_ta(Ta **ta_list_ptr, char *ta_name) {
    // first create the new Ta struct and populate
    Ta *new_ta = malloc(sizeof(Ta));
    if (new_ta == NULL) {
       perror("malloc for TA");
       exit(1);
    }
    new_ta->name = malloc(strlen(ta_name)+1);
    if (new_ta->name  == NULL) {
       perror("malloc for TA name");
       exit(1);
    }
    strcpy(new_ta->name, ta_name);
    new_ta->current_student = NULL;

    // insert into front of list
    new_ta->next = *ta_list_ptr;
    *ta_list_ptr = new_ta;
}

/* The TA ta is done with their current student.
 * Calculate the stats (the times etc.) and then
 * free the memory for the student.
 * If the TA has no current student, do nothing
 */
void release_current_student(Ta *ta) {
  Student *student = ta -> current_student;
  if(student != NULL){

    //Update stats
    Course  *course = student -> course;
    (course -> helped)++;
    course -> help_time += time(NULL) - *(student -> arrival_time);

    free(student -> arrival_time);
    free(student -> name);
    free(student);
  }
}

/* Remove this Ta from the ta_list and free the associated memory with
 * both the Ta we are removing and the current student (if any).
 * Return 0 on success or 1 if this ta_name is not found in the list
 */
int remove_ta(Ta **ta_list_ptr, char *ta_name) {
    Ta *head = *ta_list_ptr;
    if (head == NULL) {
        return 1;
    } else if (strcmp(head->name, ta_name) == 0) {
        // TA is at the head so special case
        *ta_list_ptr = head->next;
        release_current_student(head);
        // memory for the student has been freed. Now free memory for the TA.
        free(head->name);
        free(head);
        return 0;
    }
    while (head->next != NULL) {
        if (strcmp(head->next->name, ta_name) == 0) {
            Ta *ta_tofree = head->next;
            //  We have found the ta to remove, but before we do that
            //  we need to finish with the student and free the student.
            //  You need to complete this helper function
            release_current_student(ta_tofree);

            head->next = head->next->next;
            // memory for the student has been freed. Now free memory for the TA.
            free(ta_tofree->name);
            free(ta_tofree);
            return 0;
        }
        head = head->next;
    }
    // if we reach here, the ta_name was not in the list
    return 1;
}


/* Set the arrival time to current time and update course wait time stats
*  Remove pointers to other students from this student
*/
void update_student_arrival(Student *student){
  student -> course-> wait_time += (time(NULL) - *(student -> arrival_time));
  *(student-> arrival_time) = time(NULL);
  student -> next_course = NULL;
  student -> next_overall = NULL;
}


/* TA ta_name is finished with the student they are currently helping (if any)
 * and are assigned to the next student in the full queue.
 * If the queue is empty, then TA ta_name simply finishes with the student
 * they are currently helping, records appropriate statistics,
 * and sets current_student for this TA to NULL.
 * If ta_name is not in ta_list, return 1 and do nothing.
 */
int take_next_overall(char *ta_name, Ta *ta_list, Student **stu_list_ptr) {
    Ta *ta = find_ta(ta_list, ta_name);

    if(ta == NULL){
      return 1; //Ta not found
    }

    release_current_student(ta);
    ta -> current_student = *stu_list_ptr; //take the next student
    if(ta->current_student != NULL){

      //update the student list pointer, dequeue student
      dequeue_student(stu_list_ptr, ta -> current_student);

      //arrival time and stats
      update_student_arrival(ta -> current_student);
    }

    return 0;
}


/* TA ta_name is finished with the student they are currently helping (if any)
 * and are assigned to the next student in the course with this course_code.
 * If no student is waiting for this course, then TA ta_name simply finishes
 * with the student they are currently helping, records appropriate statistics,
 * and sets current_student for this TA to NULL.
 * If ta_name is not in ta_list, return 1 and do nothing.
 * If course is invalid return 2, but finish with any current student.
 */
int take_next_course(char *ta_name, Ta *ta_list, Student **stu_list_ptr, char *course_code, Course *courses, int num_courses) {
    Ta *ta = find_ta(ta_list, ta_name);
    if(ta == NULL){
      return 1;
    }
    release_current_student(ta);
    Course *course = find_course(courses, num_courses, course_code);
    if(course == NULL){
      return 2; //Invalid course
    }
    ta -> current_student =  course -> head;

    if(ta -> current_student != NULL){

      //update the student list pointer, dequeue student
      dequeue_student(stu_list_ptr, ta -> current_student);

      //arrival time and stats
      update_student_arrival(ta -> current_student);
    }

    return 0;
}


/* For each course (in the same order as in the config file), print
 * the <course code>: <number of students waiting> "in queue\n" followed by
 * one line per student waiting with the format "\t%s\n" (tab name newline)
 * Uncomment and use the printf statements below. Only change the variable
 * names.
 */
void print_all_queues(Student *stu_list, Course *courses, int num_courses) {
  int i;
  for (i = 0; i < num_courses; i++) {
    int waiting = 0;
    Course course = courses[i];
    Student *curr = course.head;
    while(curr!= NULL){ //count waiting
      waiting++;
      curr = curr ->next_course;
    }
    curr = course.head;
    while(curr!= NULL){ // now print the students
      curr = curr ->next_course;
    }
  }
}


/*
 * Print to stdout, a list of each TA, who they are serving at from what course
 * Uncomment and use the printf statements
 */
void print_currently_serving(Ta *ta_list) {
    if(ta_list == NULL){
    }else{
      Ta *curr = ta_list; //check every TA
      while (curr != NULL) {
        if(curr -> current_student == NULL){
        }else{ //TA has a student
          printf("", 0);
        }
        curr = curr-> next;
      }
    }
}


/*  list all students in queue (for testing and debugging)
 *   maybe suggest it is useful for debugging but not included in marking?
 */
void print_full_queue(Student *stu_list) {
  Student *curr = stu_list;
  while(curr != NULL){
  
    curr = curr -> next_overall;
  }

}

/* Prints statistics to stdout for course with this course_code
 * See example output from assignment handout for formatting.
 *
 */
int stats_by_course(Student *stu_list, char *course_code, Course *courses, int num_courses, Ta *ta_list) {
    Course *found = find_course(courses, num_courses, course_code);
    if(found == NULL){
      return 1;
    }
    int students_being_helped = 0;
    int students_waiting = 0;

    // count helpees
    Ta *curr_ta = ta_list;
    while (curr_ta != NULL) {
      Student *helpee = curr_ta -> current_student;
      if(helpee != NULL && helpee -> course == found){
        students_being_helped++;
      }

      curr_ta = curr_ta -> next;
    }

    //count students waiting
    Student *curr_student = found-> head;
    while(curr_student!= NULL){
      students_waiting++;
      curr_student = curr_student ->next_course;
    }




    return 0;
}


/* Dynamically allocate space for the array course list and populate it
 * according to information in the configuration file config_filename
 * Return the number of courses in the array.
 * If the configuration file can not be opened, call perror() and exit.
 */
int config_course_list(Course **courselist_ptr, char *config_filename) {
    int course_count; //output
    FILE *course_file;
    course_file = fopen(config_filename, "r");
    if(course_file == NULL){
      perror("Error opening config file");
      exit(1);
    }
    // removes and restores links
    if(fscanf(course_file, "%d ", &course_count) != 1){
      perror("Number of courses not found");
      exit(1);
    }

    char line[INPUT_BUFFER_SIZE]; //each line in the file

    *courselist_ptr = malloc_c(sizeof(Course) * course_count, "malloc course list");

    // check every line
    int i;
    for (i = 0; i < course_count; i++) {
      if(fgets(line, INPUT_BUFFER_SIZE, course_file) == NULL){
        exit(1);
      }
      Course *course = &((*courselist_ptr)[i]); //to add

      strncpy(course -> code, line , 6); //course code

      course -> description = malloc_c(sizeof(char) * (strlen(&(line[7]) +1) ),
          "malloc course description");

      strcpy(course -> description, &(line[7])); //description

      (course -> description)[strlen(course -> description) - 1] ='\0'; //remove \n

      //details - new course so all blank
      course -> helped = 0;
      course -> bailed = 0;
      course -> wait_time = 0;
      course -> help_time = 0;
      course -> head = NULL;
      course -> tail = NULL;
    }

    fclose(course_file);

    return course_count;
}