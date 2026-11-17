/* Native event and bitmap surfaces. Layout, painting and interaction stay in Fortran. */
#if defined(_WIN32) || defined(__APPLE__)
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
typedef struct {
    int kind, button, key, codepoint, mods, repeated;
    float x, y, wheel_x, wheel_y;
    char text[64];
} fw_event_t;
#endif
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <windowsx.h>
typedef struct {
    HWND window;
    fw_event_t events[2048]; int head, tail;
    unsigned char *pixels; int width, height;
    wchar_t high;
} fw_window;
static HANDLE wake_event;
static void push(fw_window *w, fw_event_t e) {
    int next = (w->tail + 1) % 2048;
    if (next == w->head) return;
    w->events[w->tail] = e; w->tail = next;
}
static wchar_t *wide(const char *text) {
    int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, -1, NULL, 0);
    wchar_t *s = n ? malloc((size_t)n * sizeof *s) : NULL;
    if (s) MultiByteToWideChar(CP_UTF8, 0, text, -1, s, n);
    return s;
}
int fw_mods(void) {
    return ((GetKeyState(VK_SHIFT) & 0x8000) ? 1 : 0) |
           ((GetKeyState(VK_CONTROL) & 0x8000) ? 2 : 0) |
           ((GetKeyState(VK_MENU) & 0x8000) ? 4 : 0) |
           (((GetKeyState(VK_LWIN)|GetKeyState(VK_RWIN)) & 0x8000) ? 8 : 0);
}
static int named(WPARAM k) {
    switch (k) {
    case VK_RETURN: return 1; case VK_ESCAPE: return 2; case VK_BACK: return 3;
    case VK_TAB: return 4; case VK_SPACE: return 5; case VK_DELETE: return 6;
    case VK_RIGHT: return 7; case VK_LEFT: return 8; case VK_DOWN: return 9;
    case VK_UP: return 10; case VK_HOME: return 11; case VK_END: return 12;
    case VK_PRIOR: return 13; case VK_NEXT: return 14; case VK_INSERT: return 15;
    default: return k >= VK_F1 && k <= VK_F24 ? 21 + (int)k - VK_F1 : 0;
    }
}
static void paint(fw_window *w, HDC dc) {
    if (!w->pixels) return;
    typedef int (WINAPI *stretch_t)(HDC,int,int,int,int,int,int,int,int,const void *,const BITMAPINFO *,UINT,DWORD);
    static stretch_t stretch;
    if (!stretch) stretch = (stretch_t)GetProcAddress(LoadLibraryW(L"gdi32.dll"), "StretchDIBits");
    if (!stretch) return;
    BITMAPINFO info; memset(&info, 0, sizeof info);
    info.bmiHeader.biSize = sizeof info.bmiHeader;
    info.bmiHeader.biWidth = w->width; info.bmiHeader.biHeight = -w->height;
    info.bmiHeader.biPlanes = 1; info.bmiHeader.biBitCount = 32;
    info.bmiHeader.biCompression = BI_RGB;
    RECT r; GetClientRect(w->window, &r);
    stretch(dc, 0,0,r.right,r.bottom,0,0,w->width,w->height,w->pixels,&info,DIB_RGB_COLORS,SRCCOPY);
}
static LRESULT CALLBACK procedure(HWND hwnd, UINT msg, WPARAM a, LPARAM b) {
    fw_window *w = (fw_window *)GetWindowLongPtrW(hwnd, GWLP_USERDATA);
    if (msg == WM_NCCREATE) {
        w = ((CREATESTRUCTW *)b)->lpCreateParams;
        w->window = hwnd;
        SetWindowLongPtrW(hwnd, GWLP_USERDATA, (LONG_PTR)w);
    }
    if (!w) return DefWindowProcW(hwnd,msg,a,b);
    fw_event_t e; memset(&e,0,sizeof e); e.mods = fw_mods();
    switch (msg) {
    case WM_CLOSE: e.kind=1; push(w,e); return 0;
    case WM_SIZE: e.kind=2; push(w,e); return 0;
    case WM_PAINT: {
        PAINTSTRUCT p; HDC dc = BeginPaint(hwnd,&p); paint(w,dc); EndPaint(hwnd,&p); return 0;
    }
    case WM_ERASEBKGND: return 1;
    case WM_MOUSEMOVE: {
        e.kind=4; e.x=GET_X_LPARAM(b); e.y=GET_Y_LPARAM(b);
        TRACKMOUSEEVENT tracking={sizeof tracking,TME_LEAVE,hwnd,0}; TrackMouseEvent(&tracking);
        break;
    }
    case WM_MOUSELEAVE: e.kind=11; break;
    case WM_LBUTTONDOWN: case WM_MBUTTONDOWN: case WM_RBUTTONDOWN:
    case WM_LBUTTONUP: case WM_MBUTTONUP: case WM_RBUTTONUP:
        e.kind = (msg == WM_LBUTTONDOWN || msg == WM_MBUTTONDOWN || msg == WM_RBUTTONDOWN) ? 5 : 6;
        e.button = (msg == WM_LBUTTONDOWN || msg == WM_LBUTTONUP) ? 1 :
                   (msg == WM_MBUTTONDOWN || msg == WM_MBUTTONUP) ? 2 : 3;
        e.x=GET_X_LPARAM(b); e.y=GET_Y_LPARAM(b);
        if(e.kind==5) { SetFocus(hwnd); SetCapture(hwnd); } else ReleaseCapture();
        break;
    case WM_MOUSEWHEEL: case WM_MOUSEHWHEEL: {
        POINT p={GET_X_LPARAM(b),GET_Y_LPARAM(b)}; ScreenToClient(hwnd,&p);
        e.x=p.x; e.y=p.y; e.kind=7;
        if(msg==WM_MOUSEWHEEL) e.wheel_y=(float)GET_WHEEL_DELTA_WPARAM(a)/WHEEL_DELTA;
        else e.wheel_x=(float)GET_WHEEL_DELTA_WPARAM(a)/WHEEL_DELTA;
        break;
    }
    case WM_KEYDOWN: case WM_SYSKEYDOWN: case WM_KEYUP: case WM_SYSKEYUP:
        e.kind=(msg==WM_KEYDOWN || msg==WM_SYSKEYDOWN)?8:9;
        e.key=named(a); e.repeated=!!(b & ((LPARAM)1<<30));
        e.codepoint=(a>='A' && a<='Z')?(int)a+32:((a>=32 && a<=126)?(int)a:0);
        break;
    case WM_CHAR: {
        wchar_t chars[3]={(wchar_t)a,0,0};
        if(a>=0xd800 && a<=0xdbff) {w->high=(wchar_t)a; return 0;}
        if(a>=0xdc00 && a<=0xdfff && w->high) {chars[0]=w->high; chars[1]=(wchar_t)a; w->high=0;}
        if(a<32 || a==127) return 0;
        e.kind=10; WideCharToMultiByte(CP_UTF8,0,chars,-1,e.text,sizeof e.text,NULL,NULL);
        break;
    }
    case WM_SETFOCUS: e.kind=12; break;
    case WM_KILLFOCUS: e.kind=13; break;
    default: return DefWindowProcW(hwnd,msg,a,b);
    }
    push(w,e); return 0;
}
void *fw_open(const char *title, int width, int height) {
    SetProcessDPIAware();
    WNDCLASSW c; memset(&c,0,sizeof c); c.lpfnWndProc=procedure;
    c.hInstance=GetModuleHandleW(NULL); c.lpszClassName=L"Fortwin";
    c.hCursor=LoadCursorW(NULL,MAKEINTRESOURCEW(32512)); RegisterClassW(&c);
    fw_window *w=calloc(1,sizeof *w); wchar_t *name=wide(title);
    if(!w || !name) {free(w);free(name);return NULL;}
    RECT r={0,0,width,height}; AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,FALSE);
    HWND hwnd=CreateWindowExW(0,L"Fortwin",name,WS_OVERLAPPEDWINDOW,CW_USEDEFAULT,CW_USEDEFAULT,
        r.right-r.left,r.bottom-r.top,NULL,NULL,c.hInstance,w); free(name);
    if(!hwnd) {free(w);return NULL;}
    if(!wake_event) wake_event=CreateEventW(NULL,FALSE,FALSE,NULL);
    return w;
}
void fw_close(void *p) { fw_window *w=p; DestroyWindow(w->window); free(w->pixels); free(w); }
int fw_event(void *p, fw_event_t *e) {
    fw_window *w=p; MSG m;
    while(PeekMessageW(&m,NULL,0,0,PM_REMOVE)) {TranslateMessage(&m); DispatchMessageW(&m);}
    if(w->head==w->tail) return 0;
    *e=w->events[w->head];w->head=(w->head+1)%2048;return 1;
}
void fw_info(void *p,int *width,int *height,float *scale) {
    fw_window *w=p;RECT r;GetClientRect(w->window,&r);*width=r.right;*height=r.bottom;
    typedef UINT (WINAPI *dpi_t)(HWND); static dpi_t dpi;
    if(!dpi) dpi=(dpi_t)GetProcAddress(GetModuleHandleW(L"user32.dll"),"GetDpiForWindow");
    *scale=dpi?(float)dpi(w->window)/96:1;
}
void fw_wait(void *p,int ms) {
    fw_window *w=p;if(w->head!=w->tail)return;
    MsgWaitForMultipleObjectsEx(1,&wake_event,ms<0?INFINITE:(DWORD)ms,QS_ALLINPUT,MWMO_INPUTAVAILABLE);
}
void *fw_handle(void *p) {return p ? ((fw_window*)p)->window : NULL;}
void fw_wake(void) {if(wake_event)SetEvent(wake_event);}
void fw_title(void *p,const char *text) {wchar_t *s=wide(text);if(s){SetWindowTextW(((fw_window*)p)->window,s);free(s);}}
void fw_size(void *p,int width,int height) {RECT r={0,0,width,height};AdjustWindowRect(&r,WS_OVERLAPPEDWINDOW,FALSE);SetWindowPos(((fw_window*)p)->window,NULL,0,0,r.right-r.left,r.bottom-r.top,SWP_NOMOVE|SWP_NOZORDER);}
void fw_visible(void *p,int visible) {ShowWindow(((fw_window*)p)->window,visible?SW_SHOW:SW_HIDE);}
void fw_maximize(void *p) {ShowWindow(((fw_window*)p)->window,SW_MAXIMIZE);}
void fw_present(void *p,const unsigned char *rgba,int width,int height) {
    fw_window *w=p;size_t n=(size_t)width*height;
    if(w->width!=width || w->height!=height) {
        unsigned char *next=realloc(w->pixels,n*4);if(!next)return;
        w->pixels=next;w->width=width;w->height=height;
    }
    for(size_t i=0;i<n;++i){w->pixels[4*i]=rgba[4*i+2];w->pixels[4*i+1]=rgba[4*i+1];w->pixels[4*i+2]=rgba[4*i];w->pixels[4*i+3]=255;}
    InvalidateRect(w->window,NULL,FALSE);UpdateWindow(w->window);
}
void fw_clipboard_set(const char *text) {
    wchar_t *s=wide(text);if(!s)return;
    size_t n=(wcslen(s)+1)*sizeof *s;HGLOBAL mem=GlobalAlloc(GMEM_MOVEABLE,n);
    if(!mem){free(s);return;}void *data=GlobalLock(mem);memcpy(data,s,n);GlobalUnlock(mem);free(s);
    if(OpenClipboard(NULL)){EmptyClipboard();if(SetClipboardData(CF_UNICODETEXT,mem))mem=NULL;CloseClipboard();}
    if(mem)GlobalFree(mem);
}
int fw_clipboard_get(char *text,int capacity) {
    int n=0;if(!OpenClipboard(NULL))return 0;
    HANDLE mem=GetClipboardData(CF_UNICODETEXT);
    if(mem){const wchar_t *s=GlobalLock(mem);if(s){n=WideCharToMultiByte(CP_UTF8,0,s,-1,NULL,0,NULL,NULL);if(capacity>=n)WideCharToMultiByte(CP_UTF8,0,s,-1,text,capacity,NULL,NULL);GlobalUnlock(mem);}}
    CloseClipboard();return n>0?n-1:0;
}
double fw_time(void) {LARGE_INTEGER t,f;QueryPerformanceCounter(&t);QueryPerformanceFrequency(&f);return (double)t.QuadPart/f.QuadPart;}
#endif
#ifdef __APPLE__
#include <dlfcn.h>
#include <time.h>
typedef void *id;
typedef void *sel_t;
typedef struct { double x,y; } point_t;
typedef struct { double width,height; } size_t2;
typedef struct { point_t origin; size_t2 size; } rect_t;
static id (*get_class)(const char *);
static sel_t (*selector)(const char *);
static void *message;
#define S(s) selector(s)
#define M0(o,s) ((id(*)(id,sel_t))message)(o,S(s))
#define M1(o,s,a) ((id(*)(id,sel_t,id))message)(o,S(s),a)
#define V0(o,s) ((void(*)(id,sel_t))message)(o,S(s))
#define V1(o,s,a) ((void(*)(id,sel_t,id))message)(o,S(s),a)
#define B1(o,s,a) ((void(*)(id,sel_t,signed char))message)(o,S(s),a)
#define L0(o,s) ((unsigned long(*)(id,sel_t))message)(o,S(s))
#define D0(o,s) ((double(*)(id,sel_t))message)(o,S(s))
static id app, mode, delegate_class;
typedef struct fw_window {
    id window, view, delegate;
    fw_event_t events[2048]; int head,tail;
    struct fw_window *next;
} fw_window;
static fw_window *windows;
void *fw_handle(void *p) {return p ? ((fw_window*)p)->window : NULL;}
static id held_event;
static id string(const char *s) {return M1(get_class("NSString"),"stringWithUTF8String:",(id)s);}
static rect_t bounds(id o, const char *sel) {
#ifdef __x86_64__
    rect_t r; void *fn=dlsym(RTLD_DEFAULT,"objc_msgSend_stret");
    ((void(*)(rect_t*,id,sel_t))fn)(&r,o,S(sel));return r;
#else
    return ((rect_t(*)(id,sel_t))message)(o,S(sel));
#endif
}
static void push(fw_window *w,fw_event_t e) {
    int next=(w->tail+1)%2048;if(next==w->head)return;
    w->events[w->tail]=e;w->tail=next;
}
static fw_window *find_window(id window) {
    for(fw_window *w=windows;w;w=w->next)if(w->window==window)return w;
    return NULL;
}
static signed char should_close(id self,sel_t sel,id window) {
    (void)self;(void)sel;fw_window *w=find_window(window);
    if(w){fw_event_t e={0};e.kind=1;push(w,e);}return 0;
}
static void resized(id self,sel_t sel,id notification) {
    (void)self;(void)sel;fw_window *w=find_window(M0(notification,"object"));
    if(w){fw_event_t e={0};e.kind=2;push(w,e);}
}
static void pump(id ev) {
    if(!ev)return;
    fw_window *w=find_window(M0(ev,"window"));
    if(!w){V1(app,"sendEvent:",ev);return;}
    unsigned long type=L0(ev,"type"),flags=L0(ev,"modifierFlags");
    fw_event_t e={0};
    e.mods=((flags&(1ul<<17))?1:0)|((flags&(1ul<<18))?2:0)|
           ((flags&(1ul<<19))?4:0)|((flags&(1ul<<20))?8:0);
    point_t pt=((point_t(*)(id,sel_t))message)(ev,S("locationInWindow"));
    rect_t r=bounds(w->view,"bounds");double scale=D0(w->window,"backingScaleFactor");
    e.x=(float)(pt.x*scale);e.y=(float)((r.size.height-pt.y)*scale);
    if(type==1||type==2||type==3||type==4||type==25||type==26) {
        if(pt.y>r.size.height){V1(app,"sendEvent:",ev);return;}
        e.kind=(type==1||type==3||type==25)?5:6;
        e.button=(type==1||type==2)?1:((type==3||type==4)?3:2);
    } else if(type==5||type==6||type==7||type==27) e.kind=4;
    else if(type==22){e.kind=7;e.wheel_y=(float)D0(ev,"scrollingDeltaY");e.wheel_x=(float)D0(ev,"scrollingDeltaX");if(L0(ev,"hasPreciseScrollingDeltas")){e.wheel_y/=10;e.wheel_x/=10;}}
    else if(type==10||type==11) {
        e.kind=type==10?8:9;e.repeated=(int)L0(ev,"isARepeat");
        id chars=M0(ev,"charactersIgnoringModifiers");
        const char *plain=(const char*)M0(chars,"UTF8String");
        unsigned short k=(unsigned short)L0(ev,"keyCode");
        switch(k){case 36:case 76:e.key=1;break;case 53:e.key=2;break;case 51:e.key=3;break;
        case 48:e.key=4;break;case 49:e.key=5;break;case 117:e.key=6;break;
        case 124:e.key=7;break;case 123:e.key=8;break;case 125:e.key=9;break;case 126:e.key=10;break;
        case 115:e.key=11;break;case 119:e.key=12;break;case 116:e.key=13;break;case 121:e.key=14;break;case 114:e.key=15;break;}
        if(plain && (unsigned char)plain[0]>=32 && (unsigned char)plain[0]<127)e.codepoint=(unsigned char)plain[0];
        push(w,e);
        if(type==10 && !(e.mods&(2|8)) && !e.key) {
            const char *text=(const char*)M0(M0(ev,"characters"),"UTF8String");
            if(text && (unsigned char)text[0]>=32){e.kind=10;strncpy(e.text,text,sizeof e.text-1);push(w,e);}
        } else if(type==10 && e.key==5 && !(e.mods&(2|8))) {
            e.kind=10;strcpy(e.text," ");push(w,e);
        }
        return;
    } else {V1(app,"sendEvent:",ev);return;}
    push(w,e);
}
void *fw_open(const char *title,int width,int height) {
    if(!app){
        if(!dlopen("/System/Library/Frameworks/AppKit.framework/AppKit",RTLD_NOW))return NULL;
        dlopen("/System/Library/Frameworks/QuartzCore.framework/QuartzCore",RTLD_NOW);
        get_class=dlsym(RTLD_DEFAULT,"objc_getClass");selector=dlsym(RTLD_DEFAULT,"sel_registerName");message=dlsym(RTLD_DEFAULT,"objc_msgSend");
        if(!get_class||!selector||!message)return NULL;
        id pool=M0(get_class("NSAutoreleasePool"),"new");
        app=M0(get_class("NSApplication"),"sharedApplication");
        ((void(*)(id,sel_t,long))message)(app,S("setActivationPolicy:"),0);
        V0(app,"finishLaunching");
        mode=M0(string("kCFRunLoopDefaultMode"),"retain");
        id (*allocate)(id,const char*,size_t)=dlsym(RTLD_DEFAULT,"objc_allocateClassPair");
        void (*register_class)(id)=dlsym(RTLD_DEFAULT,"objc_registerClassPair");
        signed char (*add)(id,sel_t,void*,const char*)=dlsym(RTLD_DEFAULT,"class_addMethod");
        delegate_class=allocate(get_class("NSObject"),"FortwinWindowDelegate",0);
        add(delegate_class,S("windowShouldClose:"),(void*)should_close,"c@:@");
        add(delegate_class,S("windowDidResize:"),(void*)resized,"v@:@");register_class(delegate_class);
        V0(pool,"drain");
    }
    fw_window *w=calloc(1,sizeof *w);if(!w)return NULL;
    id pool=M0(get_class("NSAutoreleasePool"),"new");
    rect_t screen=bounds(M0(get_class("NSScreen"),"mainScreen"),"visibleFrame");
    rect_t rect={{0,0},{fmin(width,screen.size.width*0.9),fmin(height,screen.size.height*0.9)}};
    w->window=((id(*)(id,sel_t,rect_t,unsigned long,unsigned long,signed char))message)(
        M0(get_class("NSWindow"),"alloc"),S("initWithContentRect:styleMask:backing:defer:"),rect,15,2,0);
    if(!w->window){free(w);V0(pool,"drain");return NULL;}
    B1(w->window,"setReleasedWhenClosed:",0);
    w->view=M0(w->window,"contentView");B1(w->view,"setWantsLayer:",1);
    B1(w->window,"setAcceptsMouseMovedEvents:",1);
    V1(w->window,"setTitle:",string(title));V0(w->window,"center");
    w->delegate=M0(delegate_class,"new");V1(w->window,"setDelegate:",w->delegate);
    w->next=windows;windows=w;V0(pool,"drain");return w;
}
void fw_close(void *p) {
    fw_window *w=p;fw_window **at=&windows;while(*at && *at!=w)at=&(*at)->next;
    if(*at)*at=w->next;V1(w->window,"setDelegate:",NULL);V0(w->window,"close");
    V0(w->delegate,"release");V0(w->window,"release");free(w);
}
int fw_event(void *p,fw_event_t *e) {
    fw_window *w=p;id pool=M0(get_class("NSAutoreleasePool"),"new");
    if(held_event){pump(held_event);V0(held_event,"release");held_event=NULL;}
    id ev;
    while((ev=((id(*)(id,sel_t,unsigned long,id,id,signed char))message)(app,
        S("nextEventMatchingMask:untilDate:inMode:dequeue:"),~0ul,M0(get_class("NSDate"),"distantPast"),mode,1)))pump(ev);
    V0(app,"updateWindows");V0(pool,"drain");
    if(w->head==w->tail)return 0;*e=w->events[w->head];w->head=(w->head+1)%2048;return 1;
}
void fw_info(void *p,int *width,int *height,float *scale) {
    fw_window *w=p;rect_t r=bounds(w->view,"bounds");*scale=(float)D0(w->window,"backingScaleFactor");
    *width=(int)lround(r.size.width**scale);*height=(int)lround(r.size.height**scale);
}
void fw_wait(void *p,int ms) {
    fw_window *w=p;if(w->head!=w->tail || held_event)return;
    id pool=M0(get_class("NSAutoreleasePool"),"new");
    id until=ms<0?M0(get_class("NSDate"),"distantFuture"):
        ((id(*)(id,sel_t,double))message)(get_class("NSDate"),S("dateWithTimeIntervalSinceNow:"),ms/1000.0);
    held_event=((id(*)(id,sel_t,unsigned long,id,id,signed char))message)(app,
        S("nextEventMatchingMask:untilDate:inMode:dequeue:"),~0ul,until,mode,1);
    if(held_event)V0(held_event,"retain");V0(pool,"drain");
}
void fw_wake(void) {
    if(!app)return;
    id ev=((id(*)(id,sel_t,unsigned long,point_t,unsigned long,double,long,id,short,long,long))message)(
        get_class("NSEvent"),S("otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"),
        15,(point_t){0,0},0,0.0,0,NULL,0,0,0);
    ((void(*)(id,sel_t,id,signed char))message)(app,S("postEvent:atStart:"),ev,0);
}
void fw_title(void *p,const char *s){V1(((fw_window*)p)->window,"setTitle:",string(s));}
void fw_size(void *p,int width,int height){fw_window *w=p;double scale=D0(w->window,"backingScaleFactor");((void(*)(id,sel_t,size_t2))message)(w->window,S("setContentSize:"),(size_t2){width/scale,height/scale});}
void fw_visible(void *p,int visible){fw_window *w=p;if(visible){V1(w->window,"makeKeyAndOrderFront:",NULL);B1(app,"activateIgnoringOtherApps:",1);}else V1(w->window,"orderOut:",NULL);}
void fw_maximize(void *p){V1(((fw_window*)p)->window,"zoom:",NULL);}
static void release_pixels(void *info,const void *data,size_t size){(void)info;(void)size;free((void*)data);}
void fw_present(void *p,const unsigned char *rgba,int width,int height) {
    fw_window *w=p;void *pixels=malloc((size_t)width*height*4);if(!pixels)return;
    memcpy(pixels,rgba,(size_t)width*height*4);
    void *(*space_new)(void)=dlsym(RTLD_DEFAULT,"CGColorSpaceCreateDeviceRGB");
    void *(*provider_new)(void*,const void*,size_t,void*)=dlsym(RTLD_DEFAULT,"CGDataProviderCreateWithData");
    void *(*image_new)(size_t,size_t,size_t,size_t,size_t,void*,unsigned,void*,const double*,signed char,int)=dlsym(RTLD_DEFAULT,"CGImageCreate");
    void (*space_free)(void*)=dlsym(RTLD_DEFAULT,"CGColorSpaceRelease");
    void (*provider_free)(void*)=dlsym(RTLD_DEFAULT,"CGDataProviderRelease");
    void (*image_free)(void*)=dlsym(RTLD_DEFAULT,"CGImageRelease");
    void *space=space_new(),*provider=provider_new(NULL,pixels,(size_t)width*height*4,(void*)release_pixels);
    void *image=image_new(width,height,8,32,(size_t)width*4,space,3,provider,NULL,0,0);
    id layer=M0(w->view,"layer");
    V1(layer,"setContents:",image);
    ((void(*)(id,sel_t,double))message)(layer,S("setContentsScale:"),D0(w->window,"backingScaleFactor"));
    image_free(image);provider_free(provider);space_free(space);
}
void fw_clipboard_set(const char *text){id paste=M0(get_class("NSPasteboard"),"generalPasteboard");V0(paste,"clearContents");((signed char(*)(id,sel_t,id,id))message)(paste,S("setString:forType:"),string(text),string("public.utf8-plain-text"));}
int fw_clipboard_get(char *text,int capacity){id value=M1(M0(get_class("NSPasteboard"),"generalPasteboard"),"stringForType:",string("public.utf8-plain-text"));const char *s=value?(const char*)M0(value,"UTF8String"):NULL;if(!s)return 0;size_t n=strlen(s);if(capacity>(int)n)memcpy(text,s,n+1);return (int)n;}
int fw_mods(void){unsigned long flags=L0(get_class("NSEvent"),"modifierFlags");return ((flags&(1ul<<17))?1:0)|((flags&(1ul<<18))?2:0)|((flags&(1ul<<19))?4:0)|((flags&(1ul<<20))?8:0);}
double fw_time(void){struct timespec t;clock_gettime(CLOCK_MONOTONIC,&t);return t.tv_sec+t.tv_nsec/1e9;}
#endif

#if defined(_WIN32) || defined(__APPLE__)
void fw_send_key(void *p, int codepoint, int state) {
    fw_window *w=p;
    fw_event_t e={0};e.kind=8;e.codepoint=codepoint;
    e.mods=(state&1)|((state&4)?2:0)|((state&8)?4:0)|((state&64)?8:0);
    push(w,e);e.kind=9;push(w,e);
    fw_wake();
}
#endif
