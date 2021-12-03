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

#include <unordered_map> // incidentally included when compiled with g++
#include <unordered_set>

#include <cassert>

// Everything in this file is meant to contain implementation details that a
// client coder need not see
//
// however for the purposes of testing it's important to at some level reveal
// the code

namespace tdp {

// --------------------------------- Helpers ----------------------------------

/// This defines a way to iterate and interact entries.
///
/// Essentially this is the interface for the board phase.
class IterationBase {
public:
    struct SequenceInterface {
        virtual ~SequenceInterface() {}
        virtual void prestep(FullEntry &) = 0;
        virtual void step(FullEntry &, FullEntry & other_entry) = 0;
        virtual void poststep(FullEntry &) = 0;
    };

    virtual ~IterationBase() {}

    virtual void for_each_sequence(SequenceInterface &) = 0;

    template <typename OnPairWise>
    void for_each(OnPairWise && do_pair_wise);
};

/// This describes entries, matrix, and common behaviors for all physics
/// handlers.
///
/// Implementations should inherit this class
class CollisionHandler : virtual public Physics2DHandler {
public:
    class CollisionWorker final {
    public:
        CollisionWorker(bool & b, EventRecorder & event_recorder,
            CollisionMatrix & col_matrix, EntryEntityRefMap & entries);

        CollisionWorker(const CollisionWorker & rhs) = delete;
        CollisionWorker(CollisionWorker && rhs) = delete;

        CollisionWorker & operator = (const CollisionWorker &) = delete;
        CollisionWorker & operator = (CollisionWorker &&) = delete;

        void operator () (EventHandler & event_handler, IterationBase & iteration_method);

    private:
        bool & m_was_called;
        EventRecorder & m_event_recorder;
        CollisionMatrix & m_col_matrix;
        EntryEntityRefMap & m_entries;
    };

    virtual ~CollisionHandler() {}

    // can't final the other base class though...
    void run(EventHandler &) final;

    const CollisionMatrix & collision_matrix() const final
        { return m_col_matrix; }

    void update_entry(const Entry & entry) final;

    void set_collision_matrix_(CollisionMatrix &&) final;

    // ---------------- methods used for illustrative purposes ----------------

    const int * get_push_level_for(EntityRef eref) const {
        auto itr = m_entries.find(eref);
        return itr == m_entries.end() ? nullptr : &itr->second.priority;
    }

protected:
    virtual void prepare_iteration(CollisionWorker &, EventHandler &) = 0;

    EntryMapView entries_view()
        { return View{m_entries.begin(), m_entries.end()}; }

    auto entries_view() const
        { return View{m_entries.begin(), m_entries.end()}; }

private:
    EventRecorder m_event_recorder;
    CollisionMatrix m_col_matrix;
    EntryEntityRefMap m_entries;
};

template <typename OnPairWise>
void IterationBase::for_each(OnPairWise && do_pair_wise) {
    class Impl final : public SequenceInterface {
    public:
        Impl(OnPairWise && do_pair_wise): m_f(std::move(do_pair_wise)) {}
        void prestep(FullEntry &) final {}
        void step(FullEntry & entry, FullEntry & other_entry) final
            { m_f(entry, other_entry); }
        void poststep(FullEntry &) final {}

    private:
        OnPairWise m_f;
    };
    Impl impl(std::move(do_pair_wise));
    for_each_sequence(impl);
}

} // end of tdp namespace
