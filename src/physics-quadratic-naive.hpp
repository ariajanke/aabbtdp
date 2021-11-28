/****************************************************************************

    MIT License

    Copyright (c) 2021 Aria Janke

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.

*****************************************************************************/

#pragma once

#include "helpers.hpp"

#include "detail.hpp"

namespace tdp {

namespace detail {

class Quadratic2DPhysicsImpl final : public QuadraticPhysicsHandler {
public:
    void run(EventHandler &) final;

    const CollisionMatrix & collision_matrix() const final
        { return m_info.collision_matrix(); }

    void update_entry(const Entry & entry) final
        { m_info.update_entry(entry); }

private:
    void set_collision_matrix_(CollisionMatrix && colmat) final
        { m_info.set_collision_matrix_(std::move(colmat)); }

    void find_overlaps_(const Rectangle &, const OverlapInquiry &) const final;

    EventRecorder m_event_recorder;
    TdpHandlerEntryInformation m_info;
};

} // end of detail namespace -> into ::tdp

} // end of tdp namespace
