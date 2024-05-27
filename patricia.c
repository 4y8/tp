// ========================= START PRELUDE ===================== //
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Utilitaires

#define MAX_MOTIF_LEN 1000

// Le type des chaines null-terminated contenant ACGT.
typedef char* string;

// Un vecteur de string
struct vector_s {
  int len;
  string * strs;
};
typedef struct vector_s vector;

// `allocate_vector()` aloue un vecteur vide
vector* allocate_vector() {
  string *all_lines = malloc(MAX_MOTIF_LEN * sizeof(string));
  for(int i=0;i<MAX_MOTIF_LEN;i++) { all_lines[i] = NULL; };
  vector* v = malloc(sizeof(vector));
  v->len = 0;
  v->strs = all_lines;
  return v;
}

// `add_string(v,s)` ajoute la string `s` au vecteur `v`.
void add_string(vector* v, string s) {
  v->strs[v->len] = s;
  v->len = v->len +1;
}

// `read_text(filename)` lit la chaine dans le fichier `filename` et renvoi
// une string
string read_text(char* filename) {
  FILE *fptr = fopen(filename, "r");
  
  char * line = NULL;
  size_t len = 0;
  int read = getline(&line, &len, fptr);
  if (read != -1) { return line; } { return NULL; }
}

// `read_motif(filename)` lit le motif dans le fichier `filename` et renvoi
// un vector
vector* read_motif(char* filename) {
  FILE *fptr = fopen(filename, "r");

  vector* m = allocate_vector();

  string line = NULL;  
  size_t len = 0;
  ssize_t read;

  while ((read = getline(&line, &len, fptr)) != -1) {
    if (line[read-1] == '\n') { line[read-1] = '\0'; };
    add_string(m, line);
    line = NULL;
  }
  return m ;
}

// `string_of_node(depth, pos, repr, a, c, g, t)` converti le noeud d'un patricia trie fourni en une string à l'indentation `depth`, étant donné les informations `pos` et `repr` et les strings représentant les descendants a, c, g, t
string string_of_node(int depth, unsigned pos, string repr,
                      string a, string c, string g, string t) {
  string res = malloc(8*1000);
  sprintf(res, "pos=%d repr=%s\n", pos, repr);
  char blank[depth+1];
  for (int i = 0;i<depth;i++) { blank[i]=' ';};
  blank[depth]='\0';
  if (strcmp(a,"")!=0) { strcat(res, blank); strcat(res, "└A→ "); strcat(res, a);};
  if (strcmp(c,"")!=0) { strcat(res, blank); strcat(res, "└C→ "); strcat(res, c);};
  if (strcmp(g,"")!=0) { strcat(res, blank); strcat(res, "└G→ "); strcat(res, g);};
  if (strcmp(t,"")!=0) { strcat(res, blank); strcat(res, "└T→ "); strcat(res, t);};
  return res;
}

// ========================= STOP PRELUDE ===================== //
//

struct tree_t {
        int pos;
        string repr;
        string mot;
        struct tree_t *a;
        struct tree_t *c;
        struct tree_t *g;
        struct tree_t *t;
};

typedef struct tree_t tree;

int
longest_prefix(vector *m, int pos, string *s)
{
        while (1) {
                if (m->strs[0][pos] == '\0') {
                        *s = m->strs[0];
                        return pos;
                }
                int b = 0;
                for (int j = 1; j < m->len; ++j) {
                        if (m->strs[j][pos] == '\0') {
                                *s = m->strs[j];
                                return pos;
                        }
                        if (m->strs[j][pos] != m->strs[j - 1][pos])
                                b = 1;
                }
                if (b == 1)
                        return pos;
                pos += 1;
        }
}

tree *
make_patricia_aux(vector *m, int pos)
{
        if (m->len == 0)
                return NULL;
        tree *t = malloc(sizeof(tree));
        t->mot = m->strs[0];
        t->a = NULL;
        t->c = NULL;
        t->g = NULL;
        t->t = NULL;
        if (m->len == 1) {
                t->pos = -1;
                t->repr = m->strs[0];
                return t;
        }

        t->repr = NULL;
        int u = longest_prefix(m, pos, &t->repr);
        t->pos = u;
        vector *tab[255];
        tab['A'] = allocate_vector();
        tab['C'] = allocate_vector();
        tab['G'] = allocate_vector();
        tab['T'] = allocate_vector();
        for (int i = 0; i < m->len; ++i)
                if (m->strs[i][u] != '\0')
                        add_string(tab[(int)m->strs[i][u]], m->strs[i]);
        t->a = make_patricia_aux(tab['A'], u + 1);
        t->c = make_patricia_aux(tab['C'], u + 1);
        t->g = make_patricia_aux(tab['G'], u + 1);
        t->t = make_patricia_aux(tab['T'], u + 1);
        free(tab['A']->strs);
        free(tab['C']->strs);
        free(tab['G']->strs);
        free(tab['T']->strs);
        return t;
}

tree *
make_patricia(vector *m)
{
        return make_patricia_aux(m, 0);
}

int size(tree *t) {
        if (t == NULL)
                return 0;
        return 1 + size(t->a) + size(t->c) + size(t->g) + size(t->t);
}


int
mem_aux(tree *t, string s, int pos, int n)
{
        if (t == NULL)
                return 0;
        if ((t->repr != NULL && t->pos == n) || t->pos == -1) {
                int b = 1;
                for (int i = 0; i < n + 1 && b; ++i)
                        b = s[i] == t->repr[i];
                return b;
        }

        if (t->pos == -1 || t->pos > n)
                return 0;

        for (int i = pos; i < t->pos; ++i)
                if (s[i] != t->mot[i])
                        return 0;

       if (s[t->pos] == 'A')
               return mem_aux(t->a, s, t->pos + 1, n);

       if (s[t->pos] == 'C')
               return mem_aux(t->c, s, t->pos + 1, n);

       if (s[t->pos] == 'G')
               return mem_aux(t->g, s, t->pos + 1, n);

       if (s[t->pos] == 'T')
               return mem_aux(t->t, s, t->pos + 1, n);
       return 0; // impossible
}

int
mem(tree *t, string s)
{
        return mem_aux(t, s, 0, strlen(s));
}

int
explore(string s, tree *t, int pos, int n)
{
        if (t == NULL)
                return 0;
        if (t->pos > n)
                return 0;

        if (t->pos == -1) {
                int b = 1;
                int l = strlen(t->repr);
                for (int i = 0; (i < l) && b; ++i)
                        b = s[i] == t->repr[i];
                return b;
        }

        for (int i = pos; i < t->pos; ++i)
                if (s[i] != t->mot[i])
                        return 0;

        int c = 0;

        if (s[t->pos] == 'A')
               c = explore(s, t->a, t->pos + 1, n);

        if (s[t->pos] == 'C')
               c = explore(s, t->c, t->pos + 1, n);

        if (s[t->pos] == 'G')
               c = explore(s, t->g, t->pos + 1, n);

        if (s[t->pos] == 'T')
               c = explore(s, t->t, t->pos + 1, n);

        if (t->repr != NULL)
               c += 1;

       return c;
}

int
count(string s, tree *t)
{
        int c = 0;
        int n = strlen(s);
        for (int i = 0; i < n ; ++i)
                c += explore(s + i, t, 0, n - i);
        return c;
}
int main(int argc, char *argv[]){
        printf("%d\n",count(read_text("2000/chaine_1000.txt"), make_patricia(read_motif("2000/motif_5.txt"))) % 10000);
        printf("%d\n",count(read_text("2000/chaine_1000000.txt"), make_patricia(read_motif("2000/motif_10.txt"))) % 10000);
        printf("%d\n",count(read_text("2000/chaine_5000000.txt"), make_patricia(read_motif("2000/motif_100.txt"))) % 10000);
        printf("%d\n",count(read_text("2000/chaine_1000000.txt"), make_patricia(read_motif("2000/motif_1000.txt"))) % 10000);
        return 0;
}
