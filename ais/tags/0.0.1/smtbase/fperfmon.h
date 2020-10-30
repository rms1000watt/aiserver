/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/

/* Define macros for performance timing */
/* Note: These macros are currently windows specific. */
/* Define FPERFMON in your project's Preprocessor Definitions to enable performance monitoring. */
#ifdef FPERFMON
#include <windows.h>

/* Note: place _DEF_TIMER before StartFrame (if StartFrame is used) */
#define _DEF_TIMER																	\
	double timer_Total;																\
	double timer_Temp;																\
	int timer_Running;
	LARGE_INTEGER timer_Start;														\
	LARGE_INTEGER timer_Current;													\
	LARGE_INTEGER timer_frequency;													


/* Note: place _SEED_TIMER after EndFrame (if StarFrame/EndFrame is used) */
#define _SEED_TIMER																	\
	timer_Running = 0;																\
	timer_Total = 0;																\
	QueryPerformanceFrequency(&timer_frequency);									\
	gCP->FMemory_TimerFrequency = timer_frequency.LowPart;


#define _START_TIMER QueryPerformanceCounter(&timer_Start); timer_Running = 1;

#define _STOP_TIMER																	\
	if (timer_Running == 1)															\
	{																				\
		QueryPerformanceCounter(&timer_Current);									\
		timer_Total += ((double)timer_Current.HighPart * 2,147,483,647 + timer_Current.LowPart) - ((double)timer_Start.HighPart * 2,147,483,647 + timer_Start.LowPart); \
		timer_Running = 0;															\
	}																				

#define _POST_TIMER(x)																\
	if (timer_Running == 1)															\
	{																				\
		QueryPerformanceCounter(&timer_Current);									\
		x += timer_Total + ((double)timer_Current.HighPart * 2,147,483,647 + timer_Current.LowPart) - ((double)timer_Start.HighPart * 2,147,483,647 + timer_Start.LowPart); \
	}																				\
	else																			\
	{																				\
		x += timer_Total;															\
	}																				
#else
#define _DEF_TIMER
#define _SEED_TIMER
#define _STOP_TIMER
#define _START_TIMER
#define _POST_TIMER(x) 
#endif


