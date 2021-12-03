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

#include "physics-quadratic-naive.hpp"

namespace {

using tdp::IterationBase, tdp::EntryMapView;

#if 0 // move this to an online post? (don't forget the lax copyright license!)

void must_be_called(void *);
void prework(void *);
void postwork(void *);
void setup_for_a(void *);
void setup_for_b(void *);

struct SBase {
    virtual ~SBase() {}

    class MustBeCalled final {
    public:
        MustBeCalled(bool & b): m_was_called(b) {}
        MustBeCalled(const MustBeCalled & rhs): m_was_called(rhs.m_was_called) {}
        MustBeCalled(MustBeCalled && rhs): m_was_called(rhs.m_was_called) {}
        ~MustBeCalled() {}

        MustBeCalled & operator = (const MustBeCalled &) = delete;// { throw ""; }
        MustBeCalled & operator = (MustBeCalled &&) = delete; //{ throw ""; }

        void operator () (void * ptr) {
            must_be_called(ptr);
            m_was_called = true;
        }

    private:
        bool & m_was_called;
    };

    void do_something(void * ptr) {
        prework(ptr);
        bool was_called = false;
        setup_something(MustBeCalled{was_called}, ptr);
        postwork(ptr);
    }

    virtual void setup_something(MustBeCalled &&, void * ptr) = 0;
};

struct A final : public SBase {
    void setup_something(MustBeCalled && callme, void * ptr) final {
        setup_for_a(ptr);
        callme(ptr);
    }
};

struct B final : public SBase {
    void setup_something(MustBeCalled && callme, void * ptr) final {
        setup_for_b(ptr);
        callme(ptr);
    }
};

#endif

} // end of <anonymous> namespace

namespace tdp {

void QuadraticIteration::for_each_sequence(SequenceInterface & intf) {
    for (auto & pair : m_view) {
        intf.prestep(pair.second);
        for (auto & other_pair : m_view) {
            if (pair.first == other_pair.first) continue;
            intf.step(pair.second, other_pair.second);
        }
        intf.poststep(pair.second);
    }
}

// ----------------------------------------------------------------------------

/* private */ void Quadratic2DPhysicsImpl::find_overlaps_
    (const Rectangle & rect, const OverlapInquiry & inq) const
{
    for (const auto & pair : entries_view()) {
        if (cul::find_rectangle_intersection(rect, pair.second.bounds).width > 0)
            inq(pair.second);
    }
}

/* private */ void Quadratic2DPhysicsImpl::prepare_iteration
    (CollisionWorker & do_collision_work, EventHandler & event_handler)
{
    QuadraticIteration qi{entries_view()};
    do_collision_work(event_handler, qi);
}

} // end of tdp namespace
