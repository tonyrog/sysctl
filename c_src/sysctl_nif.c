//
// sysctl nif
//

#include <stdint.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include "erl_nif.h"

// #define DEBUG

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#else
#define DBG(...)
#endif

// Atom macros
#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)			\
    atm_##name = enif_make_atom(env,string)

// Type names


static int sysctl_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int sysctl_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static int sysctl_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info);
static void sysctl_unload(ErlNifEnv* env, void* priv_data);


static ERL_NIF_TERM sysctl_get_mib(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM sysctl_set_mib(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[]);

#define SYSCTL_MAXNAME 12

ErlNifFunc sysctl_funcs[] =
{
    { "get_mib", 1, sysctl_get_mib },
    { "set_mib", 2, sysctl_set_mib },
};

static int get_mib(ErlNifEnv* env, ERL_NIF_TERM term, 
		   int* name, u_int* namelen, size_t maxlen)
{
    ERL_NIF_TERM list=term, head, tail;
    u_int i = 0;
    while((i < (u_int)maxlen) && enif_get_list_cell(env, list, &head, &tail)) {
	if (!enif_get_int(env, head, &name[i]))
	    return 0;
	i++;
	list = tail;
    }
    if ((i >= (u_int)maxlen) || !enif_is_empty_list(env, list))
	return 0;
    *namelen = i;
    return 1;
}

//
// fecth sysctl value
//
static ERL_NIF_TERM sysctl_get_mib(ErlNifEnv* env, int argc, 
			       const ERL_NIF_TERM argv[])
{
    int mib[SYSCTL_MAXNAME];
    ErlNifBinary old;
    size_t len;
    u_int n;

    if (!get_mib(env, argv[0], mib, &n, SYSCTL_MAXNAME))
	return enif_make_badarg(env);

    if (sysctl(mib, n, NULL, &len, NULL, 0) < 0)
	return enif_make_badarg(env);
    if (!enif_alloc_binary(len, &old))
	return enif_make_badarg(env);
    if (sysctl(mib, n, old.data, &len, NULL, 0) < 0) {
	enif_release_binary(&old);
	return enif_make_badarg(env);
    }
    return enif_make_binary(env, &old);
}


//
// set/update sysctl value
//
static ERL_NIF_TERM sysctl_set_mib(ErlNifEnv* env, int argc, 
				   const ERL_NIF_TERM argv[])
{
    int mib[SYSCTL_MAXNAME];
    ErlNifBinary old;
    ErlNifBinary new;
    size_t len;
    u_int n;

    if (!get_mib(env, argv[0], mib, &n, SYSCTL_MAXNAME))
	return enif_make_badarg(env);

    if (!enif_inspect_binary(env, argv[1], &new))
	return enif_make_badarg(env);

    if (sysctl(mib, n, NULL, &len, NULL, 0) < 0)
	return enif_make_badarg(env);
    
    if (!enif_alloc_binary(len, &old))
	return enif_make_badarg(env);
    
    if (sysctl(mib, n, old.data, &len, new.data, new.size) < 0) {
	enif_release_binary(&old);
	return enif_make_badarg(env);
    }
    return enif_make_binary(env, &old);
}

static int sysctl_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) load_info;

    DBG("sysctl_load\r\n");
    *priv_data = 0;
    return 0;
}

static int sysctl_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("sysctl_reload\r\n");
    return 0;
}

static int sysctl_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, 
			 ERL_NIF_TERM load_info)
{
    (void) env;
    (void) load_info;
    DBG("sysctl_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void sysctl_unload(ErlNifEnv* env, void* priv_data)
{
    (void) env;
    DBG("sysctl_unload\r\n");
}

ERL_NIF_INIT(sysctl, sysctl_funcs,
	     sysctl_load, sysctl_reload, 
	     sysctl_upgrade, sysctl_unload)




