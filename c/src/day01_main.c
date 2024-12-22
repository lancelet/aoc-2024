#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_INPUT_LINE_CHARS 100
#define MAX_LIST_LENGTH 1024
int list1[MAX_LIST_LENGTH];
int list2[MAX_LIST_LENGTH];

const char *input_file_name = "../inputs/input-day-01.txt";

int read_input_lists(const char *, int, int *, int *);
bool line_is_blank(const char *);
int compare_ints(const void *, const void *);
int part1(const int, int *, int *);
int part2(const int n_elems, const int *list1, const int *list2);

int main(void) {
  printf("Day 1\n");
  int n_elems =
      read_input_lists(input_file_name, MAX_LIST_LENGTH, list1, list2);
  if (n_elems < 0) {
    return -1;
  }
  printf("Number of input list elements = %i\n", n_elems);

  // Part 1
  int sum_diff = part1(n_elems, list1, list2);
  printf("Sum of differences: %i\n", sum_diff);

  // Part 2
  int part2_result = part2(n_elems, list1, list2);
  printf("Part 2 result: %i\n", part2_result);

  return 0;
}

int part1(const int n_elems, int *list1, int *list2) {
  // Sort both of the input lists; this mutates them.
  qsort(list1, n_elems, sizeof(int), compare_ints);
  qsort(list2, n_elems, sizeof(int), compare_ints);

  // Sum up differences.
  int sum = 0;
  for (int i = 0; i < n_elems; ++i) {
    sum += abs(list1[i] - list2[i]);
  }
  return sum;
}

int part2(const int n_elems, const int *list1, const int *list2) {
  assert(n_elems > 0);
  int total = 0;
  int list2_idx = 0;
  for (int list1_idx = 0; list1_idx < n_elems; ++list1_idx) {
    int cur_elem = list1[list1_idx];
    while (list2_idx < n_elems && list2[list2_idx] < cur_elem) {
      ++list2_idx;
    }
    int count = 0;
    while ((list2_idx + count) < n_elems &&
           list2[list2_idx + count] == cur_elem) {
      ++count;
    }
    total += count * cur_elem;
    list2_idx += count;
  }
  return total;
}

int compare_ints(const void *va, const void *vb) {
  int a = *(int *)va;
  int b = *(int *)vb;
  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
}

/**** PARSING ****************************************************************/

int read_input_lists(const char *input_file_name, int max_list_length,
                     int *list1, int *list2) {
  FILE *in = fopen(input_file_name, "r");
  if (in == NULL) {
    perror("Could not open file");
    return -1;
  }

  int n_lines_read = 0;
  char line[MAX_INPUT_LINE_CHARS];
  while (fgets(line, sizeof(line), in)) {
    if (line_is_blank(line)) {
      continue;
    }
    if (n_lines_read >= max_list_length) {
      fprintf(stderr, "Input file exceeded maximum list length.\n");
      fclose(in);
      return -1;
    }
    int a, b;
    if (sscanf(line, "%d %d", &a, &b) == 2) {
      list1[n_lines_read] = a;
      list2[n_lines_read] = b;
      ++n_lines_read;
    } else {
      fprintf(stderr, "Could not read two int values from line.\n");
      fclose(in);
      return -1;
    }
  }
  if (ferror(in)) {
    perror("Error reading from file");
    fclose(in);
    return -1;
  }

  fclose(in);
  assert(n_lines_read < max_list_length);
  return n_lines_read;
}

/**
 * @brief Checks if a given string contains only whitespace characters or is
 * empty.
 *
 * This function iterates through each character in the input string and
 * verifies if all characters are whitespace (as determined by `isspace`) or
 * if the string is empty. A string with only whitespace characters or no
 * characters is considered blank.
 *
 * @param line A pointer to a null-terminated string to check.
 * @return true if the string is blank (only whitespace or empty).
 * @return false if the string contains any non-whitespace character.
 *
 * @note The input string must be null-terminated.
 * @warning The behavior is undefined if `line` is a NULL pointer.
 *
 * @example
 * ```
 * if (line_is_blank("   \t\n")) {
 *     printf("The line is blank.\n");
 * }
 * ```
 *
 * @see isspace
 */
bool line_is_blank(const char *line) {
  assert(line != NULL);
  while (*line != '\0') {
    if (!isspace(*line)) {
      return false;
    }
    line++;
  }
  return true;
}
