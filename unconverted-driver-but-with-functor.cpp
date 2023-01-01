/*******************************************************
 * Copyright (c) 2014, ArrayFire
 * All rights reserved.
 *
 * This file is distributed under 3-clause BSD license.
 * The complete license agreement can be obtained at:
 * http://arrayfire.com/licenses/BSD-3-Clause
 ********************************************************/

#pragma once

#include <Param.hpp>
#include <common/dispatch.hpp>
#include <common/kernel_cache.hpp>
#include <debug_oneapi.hpp>
#include <traits.hpp>

#include <string>
#include <vector>

namespace oneapi {
namespace kernel {

template<typename T>
using local_accessor = sycl::accessor<T, 1, sycl::access::mode::read_write,
                                      sycl::access::target::local>;
template<typename T>
using read_accessor = sycl::accessor<T, 1, sycl::access::mode::read>;
template<typename T>
using write_accessor = sycl::accessor<T, 1, sycl::access::mode::write>;

template<typename T>
class tileCreateKernel {public:
    tileCreateKernel(read_accessor<T> out, read_accessor<T> in, const KParam op, const KParam ip, const int blocksPerMatX, const int blocksPerMatY) : out_(out), in_(in), op_(op), ip_(ip), blocksPerMatX_(blocksPerMatX), blocksPerMatY_(blocksPerMatY) {}
    void operator()(sycl::nd_item<2> it) const {
        sycl::group g = it.get_group();

    const int oz = g.get_group_id(0) / blocksPerMatX;
    const int ow = g.get_group_id(1) / blocksPerMatY;

    const int blockIdx_x = g.get_group_id(0) - oz * blocksPerMatX;
    const int blockIdx_y = g.get_group_id(1) - ow * blocksPerMatY;

    const int xx = it.get_local_id(0) + blockIdx_x * g.get_local_range(0);
    const int yy = it.get_local_id(1) + blockIdx_y * g.get_local_range(1);

    if (xx >= op.dims[0] || yy >= op.dims[1] || oz >= op.dims[2] ||
        ow >= op.dims[3])
        return;

    const int iz  = oz % ip.dims[2];
    const int iw  = ow % ip.dims[3];
    const int izw = iw * ip.strides[3] + iz * ip.strides[2];
    const int ozw = ow * op.strides[3] + oz * op.strides[2];

    const int incy = blocksPerMatY * g.get_local_range(1);
    const int incx = blocksPerMatX * g.get_local_range(0);

    for (int oy = yy; oy < op.dims[1]; oy += incy) {
        const int iy = oy % ip.dims[1];
        for (int ox = xx; ox < op.dims[0]; ox += incx) {
            const int ix = ox % ip.dims[0];

            int iMem = izw + iy * ip.strides[1] + ix;
            int oMem = ozw + oy * op.strides[1] + ox;

            out[oMem] = in[ip.offset + iMem];
        }
    }
}

private:
read_accessor<T> out_;
read_accessor<T> in_;
const KParam op_;
const KParam ip_;
const int blocksPerMatX_;
const int blocksPerMatY_;
};





template<typename T>
void tile(Param out, const Param in) {
    using cl::EnqueueArgs;
    using cl::NDRange;
    using std::string;
    using std::vector;

    constexpr int TX    = 32;
    constexpr int TY    = 8;
    constexpr int TILEX = 512;
    constexpr int TILEY = 32;

    vector<TemplateArg> targs = {
        TemplateTypename<T>(),
    };
    vector<string> compileOpts = {
        DefineKeyValue(T, dtype_traits<T>::getName()),
    };
    compileOpts.emplace_back(getTypeBuildDefinition<T>());

    auto tile =
        common::getKernel("tile", std::array{tile_cl_src}, targs, compileOpts);

    NDRange local(TX, TY, 1);

    int blocksPerMatX = divup(out.info.dims[0], TILEX);
    int blocksPerMatY = divup(out.info.dims[1], TILEY);
    NDRange global(local[0] * blocksPerMatX * out.info.dims[2],
                   local[1] * blocksPerMatY * out.info.dims[3], 1);

    tile(EnqueueArgs(getQueue(), global, local), *out.data, *in.data, out.info,
         in.info, blocksPerMatX, blocksPerMatY);
    CL_DEBUG_FINISH(getQueue());
}
}  // namespace kernel
}  // namespace oneapi
