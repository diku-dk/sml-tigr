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
