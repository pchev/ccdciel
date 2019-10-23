/* File pasraw.cpp
   
   Shared library for using Libraw from Pascal program
   
   The function is limited to get raw non processed image
   to help convertion to FITS
   
*/

#include "libraw/libraw.h"

struct TImgInfo
{
   int rawwidth;
   int rawheight;
   int imgwidth;
   int imgheight;
   int topmargin;
   int leftmargin;
   char bayerpattern[4];
   unsigned short *bitmap;
};

LibRaw RawProcessor;
#define S RawProcessor.imgdata.sizes
#define P RawProcessor.imgdata.idata

extern "C" int loadraw(char *rawinput, int inputsize)
{
    int ret;
    if (ret = RawProcessor.open_buffer(rawinput, inputsize) != LIBRAW_SUCCESS)
    {
      return(ret);
    }
    if ((ret = RawProcessor.unpack()) != LIBRAW_SUCCESS)
    {
      return(ret);
    }
    ret = RawProcessor.adjust_sizes_info_only();

    if (!(P.filters || P.colors == 1)) 
    {
      return(-1);
    }
    return(0);
}

extern "C" int closeraw()
{
    RawProcessor.recycle(); 
    return(0);
}    

extern "C" int getinfo(TImgInfo *info)
{
   if ((!S.raw_width)&&(!S.raw_height)) {
     return(1);
   }
   info->rawwidth = S.raw_width;
   info->rawheight = S.raw_height;
   info->imgwidth = S.width;
   info->imgheight = S.height;
   info->topmargin = S.top_margin;
   info->leftmargin = S.left_margin;
   info->bitmap = RawProcessor.imgdata.rawdata.raw_image;
   if (P.filters)
   {
     if (!P.cdesc[3])
       P.cdesc[3] = 'G';
     info->bayerpattern[0] = P.cdesc[RawProcessor.COLOR(0, 0)];
     info->bayerpattern[1] = P.cdesc[RawProcessor.COLOR(0, 1)];
     info->bayerpattern[2] = P.cdesc[RawProcessor.COLOR(1, 0)];
     info->bayerpattern[3] = P.cdesc[RawProcessor.COLOR(1, 1)];
   }
   return(0);
}    

extern "C"  void geterrormsg(int ret, char *msg)
{
    sprintf(msg,"%s",libraw_strerror(ret));
}   
