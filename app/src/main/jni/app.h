#ifndef APP_H_INCLUDED
#define APP_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

extern void appInit(char *logfile);
extern void appDeinit();
extern void appEval(char *code);

#ifdef __cplusplus
}
#endif


#endif // !APP_H_INCLUDED
