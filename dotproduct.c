#include <emacs-module.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int plugin_is_GPL_compatible;

static double
get_at (emacs_env *env, emacs_value vector, ptrdiff_t index)
{
  emacs_value a = env->vec_get (env, vector, index);
  return env->extract_float (env, a);
}

static emacs_value
make_vector (emacs_env *env, emacs_value *values, ptrdiff_t size)
{
  emacs_value vector = env->intern(env, "vector");

  emacs_value result = env->funcall(env, vector, size, values);
  free(values);

  return result;
}

static void
message_index (emacs_env *env, int i)
{
  char message[100];
  sprintf(message, "Comparing embedding %d", i);

  emacs_value data = env->make_string (env, message, strlen (message));

  env->funcall (env, env->intern (env, "message"), 1, &data);
}

static emacs_value
dot_product (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
            void *data)
{
  assert (nargs == 2);
  emacs_value a = args[0];
  emacs_value b = args[1];

  ptrdiff_t size = env->vec_size (env, a);

  double dp = 0;
  for (int i = 0; i < size; i++) {
    double first = get_at(env, a, i);
    double second = get_at(env, b, i);
    dp += first * second;
  }

  emacs_value result = env->make_float (env, dp);
  return result;
}

static emacs_value
cosine_similarity (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  assert (nargs == 2);
  emacs_value a = args[0];
  emacs_value b = args[1];

  emacs_value dot = dot_product(env, 2, args, data);
  emacs_value param_a[] = {a, a};
  emacs_value norm_a = dot_product(env, 2, param_a, data);
  emacs_value param_b[] = {b, b};
  emacs_value norm_b = dot_product(env, 2, param_b, data);

  double f_dot = env->extract_float(env, dot);
  double f_norm_a = env->extract_float(env, norm_a);
  double f_norm_b = env->extract_float(env, norm_b);

  double cos = f_dot / (sqrt(f_norm_a) * sqrt(f_norm_b));

  emacs_value result = env->make_float(env, cos);
  return result;
}

static emacs_value
compute_score (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
            void *data)
{
  assert (nargs == 2);
  emacs_value query = args[0];
  emacs_value embeddings = args[1];

  ptrdiff_t size = env->vec_size (env, embeddings);
  emacs_value *scores = malloc(sizeof(emacs_value) * size);
  // compute cosine similarity for each embedding against query
  for (int i = 0; i < size; i++) {
    if (i % 100 == 0) {
      message_index(env, i);
    }
    emacs_value embedding = env->vec_get (env, embeddings, i);
    emacs_value param[] = {embedding, query};
    emacs_value score = cosine_similarity(env, 2, param, data);
    scores[i] = score;
  }

  return make_vector(env, scores, size);
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);

  emacs_value symbol;
  emacs_value func;

  symbol = env->intern (env, "org-graph-dot");
  func = env->make_function (env, 2, 2, dot_product, "Compute dot product of A and B.", NULL);
  emacs_value args[] = {symbol, func};
  env->funcall (env, env->intern (env, "defalias"), 2, args);

  symbol = env->intern (env, "org-graph-cosine-similarity");
  func = env->make_function (env, 2, 2, cosine_similarity, "Compute cosine similarity between A and B", NULL);
  emacs_value args_2[] = {symbol, func};
  env->funcall (env, env->intern (env, "defalias"), 2, args_2);

  symbol = env->intern (env, "org-graph-compute-score");
  func = env->make_function (env, 2, 2, compute_score, "Compute score of QUERY against EMBEDDINGS.", NULL);
  emacs_value args_3[] = {symbol, func};
  env->funcall (env, env->intern (env, "defalias"), 2, args_3);

  return 0;
}
