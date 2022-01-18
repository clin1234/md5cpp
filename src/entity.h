/* md5cpp: fast Markdown parser in C++20.
Copyright 2021 Charlie Lin
This software is derived from md4c, and is licensed under the same terms as
md4c, reproduced below:
*/
/*
 * MD4C: Markdown parser for C
 * (http://github.com/mity/md4c)
 *
 * Copyright (c) 2016-2019 Martin Mitas
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#ifndef MD4C_ENTITY_H
#define MD4C_ENTITY_H

#ifdef __cplusplus
#include <optional>
#include <string_view>
#endif

struct entity {
  const char *name;
  unsigned codepoints[2];
};

#ifdef __cplusplus
/* Returns `std::optional<struct entity>`, where `operator bool()` can be used
to determine if `name` matches an entity, and .value() if it exists. */
constexpr std::optional<entity> lookup(std::string_view);

extern "C" {
#endif // __cplusplus
const struct entity *entity_lookup(const char *name, size_t name_size);
#ifdef __cplusplus
}
#endif // __cplusplus
#endif /* MD4C_ENTITY_H */
