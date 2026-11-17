#if !defined(_WIN32) && !defined(__APPLE__) && !defined(__EMSCRIPTEN__)
/* X11 image transport. Rendering and window lifetime belong to Fortran. */
#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
typedef struct {
  unsigned int sequence;
} cookie;
static void *library;
static uint32_t (*generate_id)(void *);
static cookie (*create_gc)(void *, uint32_t, uint32_t, uint32_t,
                           const uint32_t *);
static cookie (*put_image)(void *, uint8_t, uint32_t, uint32_t, uint16_t,
                           uint16_t, int16_t, int16_t, uint8_t, uint8_t,
                           uint32_t, const uint8_t *);
static cookie (*free_gc)(void *, uint32_t);
static int (*flush)(void *);
static cookie (*create_pixmap)(void *, uint8_t, uint32_t, uint32_t, uint16_t,
                               uint16_t);
static cookie (*free_pixmap)(void *, uint32_t);
static cookie (*attributes)(void *, uint32_t, uint32_t, const uint32_t *);
static cookie (*clear_area)(void *, uint8_t, uint32_t, int16_t, int16_t,
                            uint16_t, uint16_t);
static int load(void) {
  if (library)
    return 1;
  void *lib = dlopen("libxcb.so.1", RTLD_NOW | RTLD_LOCAL);
  if (!lib)
    return 0;
#define LOAD(target, name)                                                     \
  do {                                                                         \
    *(void **)(&target) = dlsym(lib, name);                                    \
    if (!target) {                                                             \
      dlclose(lib);                                                            \
      return 0;                                                                \
    }                                                                          \
  } while (0)
  LOAD(generate_id, "xcb_generate_id");
  LOAD(create_gc, "xcb_create_gc");
  LOAD(put_image, "xcb_put_image");
  LOAD(free_gc, "xcb_free_gc");
  LOAD(flush, "xcb_flush");
  LOAD(create_pixmap, "xcb_create_pixmap");
  LOAD(free_pixmap, "xcb_free_pixmap");
  LOAD(attributes, "xcb_change_window_attributes");
  LOAD(clear_area, "xcb_clear_area");
  library = lib;
  return 1;
#undef LOAD
}
void fortwin_xcb_pixels(void *connection, uint32_t window, const uint8_t *rgba,
                        int width, int height, int depth) {
  if (!connection || !window || width <= 0 || height <= 0 || depth != 24 ||
      !load())
    return;
  /* Each request fits the core protocol's 256 KiB limit, including its header.
   */
  int rows = 65500 / (4 * width);
  if (rows < 1)
    return;
  if (rows > height)
    rows = height;
  uint32_t *pixels = malloc((size_t)rows * width * 4);
  if (!pixels)
    return;
  uint32_t gc = generate_id(connection), pixmap = generate_id(connection),
           zero = 0;
  create_pixmap(connection, 24, pixmap, window, (uint16_t)width,
                (uint16_t)height);
  create_gc(connection, gc, pixmap, 1u << 16, &zero);
  for (int y = 0; y < height; y += rows) {
    int n = height - y;
    if (n > rows)
      n = rows;
    for (int k = 0; k < n * width; ++k) {
      const uint8_t *p = rgba + 4 * ((size_t)y * width + k);
      pixels[k] = ((uint32_t)p[0] << 16) | ((uint32_t)p[1] << 8) | p[2];
    }
    put_image(connection, 2, pixmap, gc, (uint16_t)width, (uint16_t)n, 0,
              (int16_t)y, 0, 24, (uint32_t)(n * width * 4),
              (const uint8_t *)pixels);
  }
  /* A hidden window maps directly to its completed first frame. */
  attributes(connection, window, 1, &pixmap);
  clear_area(connection, 0, window, 0, 0, (uint16_t)width, (uint16_t)height);
  free_gc(connection, gc);
  free_pixmap(connection, pixmap);
  flush(connection);
  free(pixels);
}
#endif
