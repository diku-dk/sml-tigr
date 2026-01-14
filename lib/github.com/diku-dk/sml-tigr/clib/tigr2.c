#include "tigr.h"

// Extra TIGR functions

TigrFont* tigrFont() {
  return tfont;
}

int tigrWidth(Tigr* bmp) {
  return bmp->w;
}

int tigrHeight(Tigr* bmp) {
  return bmp->h;
}

int toCharArray(Tigr* bmp, int len, char* a) {
  int h = bmp->h;
  int w = bmp->w;
  int sz = len / 4;
  if ( len < 0 || len != sz*4 || h*w != sz ) {
    return -1;
  }
  for (int r = 0; r < h; r++) {
    for (int c = 0; c < w; c++) {
      int i = r*w+c;
      TPixel p = bmp->pix[i];
      a[4*i] = p.r;
      a[4*i+1] = p.g;
      a[4*i+2] = p.b;
      a[4*i+3] = p.a;
    }
  }
  return 0;
}

int fromCharArray(Tigr* bmp, int len, char* a) {
  int h = bmp->h;
  int w = bmp->w;
  int sz = len / 4;
  if ( len < 0 || len != sz*4 || h*w != sz ) {
    return -1;
  }
  TPixel* pix = bmp->pix;
  for (int r = 0; r < h; r++) {
    for (int c = 0; c < w; c++) {
      int i = r*w+c;
      pix[i].r = a[4*i];
      pix[i].g = a[4*i+1];
      pix[i].b = a[4*i+2];
      pix[i].a = a[4*i+3];
    }
  }
  return 0;
}
