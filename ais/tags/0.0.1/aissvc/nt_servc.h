/**
  @file

  @brief
  Windows NT Service class library

  Copyright Abandoned 1998 Irena Pancirov - Irnet Snc
  This file is public domain and comes with NO WARRANTY of any kind
*/

// main application thread
typedef void (*THREAD_FC)(void *);

class NTService
{
  public:
    NTService();
   ~NTService();

    BOOL    bOsNT;	      ///< true if OS is NT, false for Win95
    //install optinos
    DWORD   dwDesiredAccess;
    DWORD   dwServiceType;
    DWORD   dwStartType;
    DWORD   dwErrorControl;

    LPTSTR   szLoadOrderGroup;
    LPDWORD lpdwTagID;
    LPTSTR   szDependencies;
    OSVERSIONINFO osVer;

    // time-out (in milisec)
    int     nStartTimeOut;
    int     nStopTimeOut;
    int     nPauseTimeOut;
    int     nResumeTimeOut;

    //
    DWORD   my_argc;
    LPTSTR *my_argv;
    HANDLE  hShutdownEvent;
    int     nError;
    DWORD   dwState;

    BOOL GetOS();	      // returns TRUE if WinNT
    BOOL IsNT() { return bOsNT;}
    //init service entry point
    long Init(LPCTSTR szInternName,void *ServiceThread);

    //application shutdown event
    void SetShutdownEvent(HANDLE hEvent){ hShutdownEvent=hEvent; }
	//application shutdown handler
	void SetShutdownHandler(void *StopHandler){ fpStopHandler = (THREAD_FC)StopHandler; }


    //service install / un-install
    BOOL Install(int startType,LPCTSTR szInternName,LPCTSTR szDisplayName,
                 LPCTSTR szFullPath, LPCTSTR szAccountName=NULL,
                 LPCTSTR szPassword=NULL);
    BOOL SeekStatus(LPCTSTR szInternName, int OperationType);
    BOOL Remove(LPCTSTR szInternName);
    BOOL IsService(LPCTSTR ServiceName);
	
    BOOL got_service_option(char **argv, char *service_option);
    BOOL is_super_user();
    void Stop(void); //to be called from app. to stop service

	BOOL IsRunning(LPCTSTR ServiceName);

	void Start(LPCTSTR ServiceName);
	void Stop(LPCTSTR ServiceName);

  protected:
    LPTSTR		   ServiceName;
    HANDLE		   hExitEvent;
    SERVICE_STATUS_HANDLE  hServiceStatusHandle;
    BOOL		   bPause;
    BOOL		   bRunning;
    HANDLE		   hThreadHandle;
    THREAD_FC	   fpServiceThread;
	THREAD_FC	   fpStopHandler;

    void PauseService();
    void ResumeService();
    void StopService();
    BOOL StartService();

    static void ServiceMain(DWORD argc, LPTSTR *argv);
    static void ServiceCtrlHandler (DWORD ctrlCode);

    void Exit(DWORD error);
    BOOL SetStatus (DWORD dwCurrentState,DWORD dwWin32ExitCode,
		    DWORD dwServiceSpecificExitCode,
		    DWORD dwCheckPoint,DWORD dwWaitHint);

};
/* ------------------------- the end -------------------------------------- */
