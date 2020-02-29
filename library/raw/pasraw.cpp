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

struct TImgInfo2
{
   int version;
   char camera[80];
   time_t timestamp;
   int isospeed;
   float shutter;
   float aperture;
   float focal_len;
   int colors;
   float rmult;
   float gmult;
   float bmult;
   float temperature;
};

LibRaw RawProcessor;
#define S  RawProcessor.imgdata.sizes
#define P1 RawProcessor.imgdata.idata
#define P2 RawProcessor.imgdata.other
#define C  RawProcessor.imgdata.color
#if (LIBRAW_COMPILE_CHECK_VERSION_NOTLESS(0, 20))
#define P3 RawProcessor.imgdata.makernotes.common
#endif



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

    if (!(P1.filters || P1.colors == 1))
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
   if (P1.filters)
   {
     if (!P1.cdesc[3])
       P1.cdesc[3] = 'G';
     info->bayerpattern[0] = P1.cdesc[RawProcessor.COLOR(0, 0)];
     info->bayerpattern[1] = P1.cdesc[RawProcessor.COLOR(0, 1)];
     info->bayerpattern[2] = P1.cdesc[RawProcessor.COLOR(1, 0)];
     info->bayerpattern[3] = P1.cdesc[RawProcessor.COLOR(1, 1)];
   }
   return(0);
}

extern "C" int getinfo2(TImgInfo2 *info)
{
   if ((!S.raw_width)&&(!S.raw_height)) {
     return(1);
   }
   int RequestVersion = info->version;
   info->version = 3;
   snprintf(info->camera, 80, "%s %s", P1.make, P1.model);
   info->timestamp = P2.timestamp;
   info->isospeed = (int)P2.iso_speed;
   info->shutter = P2.shutter;
   info->aperture = P2.aperture;
   info->focal_len = P2.focal_len;
   info->colors = P1.colors;
   if (P1.colors == 3) {
     info->rmult = C.pre_mul[0];
     info->gmult = C.pre_mul[1];
     info->bmult = C.pre_mul[2];
   } else {
     info->rmult = 1.0;
     info->gmult = 1.0;
     info->bmult = 1.0;
   }
   if (RequestVersion >= 3){
#if (LIBRAW_COMPILE_CHECK_VERSION_NOTLESS(0, 20))
     // version 0.20 , temperature is in imgdata.makernotes.common
     if (P3.SensorTemperature > -273.15f) {
       info->temperature = P3.SensorTemperature;
     }
     else if (P3.SensorTemperature2 > -273.15f) {
       info->temperature = P3.SensorTemperature2;
     }
     else if (P3.CameraTemperature > -273.15f) {
       info->temperature = P3.CameraTemperature;
     }
     else if (P3.AmbientTemperature > -273.15f) {
       info->temperature = P3.AmbientTemperature;
     }
     else if (P3.exifAmbientTemperature > -273.15f) {
       info->temperature = P3.exifAmbientTemperature;
     }
     else {
       info->temperature = -999.0f;
     }
#else
#if (LIBRAW_COMPILE_CHECK_VERSION_NOTLESS(0, 19))
     // version 0.19 , temperature is in imgdata.other
     if (P2.SensorTemperature > -273.15f) {
       info->temperature = P2.SensorTemperature;
     }
     else if (P2.SensorTemperature2 > -273.15f) {
       info->temperature = P2.SensorTemperature2;
     }
     else if (P2.CameraTemperature > -273.15f) {
       info->temperature = P2.CameraTemperature;
     }
     else if (P2.AmbientTemperature > -273.15f) {
       info->temperature = P2.AmbientTemperature;
     }
     else if (P2.exifAmbientTemperature > -273.15f) {
       info->temperature = P2.exifAmbientTemperature;
     }
     else {
       info->temperature = -999.0f;
     }
#else
    // no temperature in previous version
    info->temperature = -999.0f;
#endif
#endif
   }
   return(0);
}

extern "C"  void geterrormsg(int ret, char *msg)
{
    sprintf(msg,"%s",libraw_strerror(ret));
}   
