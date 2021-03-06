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

#include <common/TestSuite.hpp>
#include <common/Vector2Util.hpp>

#include <aabbtdp/defs.hpp>
#include <aabbtdp/physics.hpp>

#include <numeric>

#include <cassert>

namespace {


using cul::ts::TestSuite, cul::ts::Unit, cul::Grid, cul::overlaps, cul::ts::test,
      std::make_tuple;
using namespace tdp;

// cannot include "helpers.hpp"
Rectangle displace(Rectangle rv, Vector r) {
    rv.left += r.x;
    rv.top  += r.y;
    return rv;
}

constexpr const int k_solid   = 0;
constexpr const int k_sensor  = 1;
constexpr const int k_passive = 2;

auto make_collision_matrix() {
    using namespace interaction_classes;
    return Grid( {
    //         solid,        sensor,     passive
        { k_as_solid, k_as_trespass, k_as_passive }, // solid
        { k_reflect , k_as_trespass, k_as_passive }, // sensor
        { k_reflect , k_reflect    , k_as_passive }  // passive
    });
}

auto set_to_default_collision_matrix(std::unique_ptr<Physics2DHandler> handler) {
    handler->set_collision_matrix(make_collision_matrix());
    return handler;
}

class EntryMaker final {
public:
    explicit EntryMaker(Entry * entry): m_entry(entry) {}

    EntryMaker & set_bounds(Rectangle rect) {
        m_entry->bounds = rect;
        return *this;
    }

    EntryMaker & set_layer(int layer) {
        m_entry->collision_layer = layer;
        return *this;
    }

    EntryMaker & set_displacement(Vector r) {
        m_entry->displacement = r;
        return *this;
    }

    EntryMaker & make_pushable() {
        m_entry->pushable = true;
        return *this;
    }

    Entry & operator () () const { return *m_entry; }

private:
    Entry * m_entry;
};

class EntryMakerMaker final {
public:
    EntryMakerMaker() {}

    EntryMaker add_entry() {
        assert(m_for_new != m_entries.data() + m_entries.size());
        auto ptr = m_for_new++;
        ptr->name   = 'a' + (ptr - m_entries.data());
        ptr->entity = ptr;
        return EntryMaker{ptr};
    }

    Entry & to_entry(void * ptr) const {
        return to_full_entry(ptr);
    }

    char get_name(void * ptr) const {
        return to_full_entry(ptr).name;
    }

private:
    struct NamedEntry final : public Entry {
        char name;
    };

    NamedEntry & to_full_entry(void * ptr) const {
        assert(ptr >= m_entries.data() && ptr < (m_entries.data() + m_entries.size()));
        return *reinterpret_cast<NamedEntry *>(ptr);
    }

    std::array<NamedEntry, 26> m_entries;
    NamedEntry * m_for_new = m_entries.data();
};

auto make_null_event_handler(const EntryMakerMaker & emm) {
    class Impl final : public EventHandler {
    public:
        Impl(const EntryMakerMaker & emm):
            m_emm(emm) {}

        bool check_accept_collision(Entity, Entity) const final { return true; }

        void on_collision(Entity, Entity, bool, OccurrenceType) final {}

        void on_trespass(Entity, Entity, OccurrenceType) final {}

        void finalize_entry(Entity a, Rectangle new_bounds)
            { m_emm.to_entry(a).bounds = new_bounds; }

    private:
        const EntryMakerMaker & m_emm;
    };
    return Impl{emm};
}

template <typename Func>
auto make_trespass_checker
    (const EntryMakerMaker & emm, Func && f)
{
    class Impl final : public EventHandler {
    public:
        Impl(Func && f, const EntryMakerMaker & emm):
            m_emm(emm), m_f(std::move(f)) {}

        bool check_accept_collision(Entity, Entity) const final { return true; }

        void on_collision(Entity, Entity, bool, OccurrenceType) final {}

        void on_trespass(Entity a, Entity b, OccurrenceType otype) final {
            auto ae = m_emm.to_entry(a);
            auto be = m_emm.to_entry(b);
            m_f(ae, be, otype);
        }

        void finalize_entry(Entity a, Rectangle new_bounds)
            { m_emm.to_entry(a).bounds = new_bounds; }

    private:
        const EntryMakerMaker & m_emm;
        Func m_f;
    };
    return Impl{std::move(f), emm};
}

template <typename Func>
auto make_collision_checker(const EntryMakerMaker & emm, Func && f) {
    class Impl final : public EventHandler {
    public:
        Impl(Func && f, const EntryMakerMaker & emm):
            m_emm(emm), m_f(std::move(f)) {}

        bool check_accept_collision(Entity, Entity) const final { return true; }

        void on_collision(Entity a, Entity b, bool is_pushing, OccurrenceType) final {
            auto ae = m_emm.to_entry(a);
            auto be = m_emm.to_entry(b);
            m_f(ae, be, is_pushing);
        }

        void on_trespass(Entity, Entity, OccurrenceType) final {}

        void finalize_entry(Entity a, Rectangle new_bounds)
            { m_emm.to_entry(a).bounds = new_bounds; }

    private:
        const EntryMakerMaker & m_emm;
        Func m_f;
    };
    return Impl{std::move(f), emm};
}

#define mark MACRO_MARK_POSITION_OF_CUL_TEST_SUITE

} // end of <anonymous> namespace

using HandlerMaker = std::unique_ptr<Physics2DHandler>(*)();

void do_first_frame_trespass(TestSuite & suite, HandlerMaker);

void do_head_on_collision(TestSuite & suite, HandlerMaker);

void do_multiple_collision_bug(TestSuite & suite, HandlerMaker);

void do_bad_entity_reference(TestSuite & suite, HandlerMaker);

void do_trespass_recording(TestSuite & suite, HandlerMaker);

void do_integration_tests(TestSuite & suite) {
    // three primary tests one for each issue
    // first frame trespass
    // head on collision
    // multiple collision events for pushed
    suite.start_series("integration tests");

    auto make_default_handler = [] {
        return set_to_default_collision_matrix(QuadraticPhysicsHandler::make_instance());
    };

    do_first_frame_trespass(suite, make_default_handler);
    do_head_on_collision(suite, make_default_handler);
    do_head_on_collision(suite, [] {
        return set_to_default_collision_matrix(SweepSwitchPhysicsHandler::make_instance());
    });
    do_multiple_collision_bug(suite, make_default_handler);
    do_bad_entity_reference(suite, make_default_handler);
    do_trespass_recording(suite, make_default_handler);
}

void do_first_frame_trespass(TestSuite & suite, HandlerMaker make_handler_) {
    static thread_local HandlerMaker make_handler;
    make_handler = make_handler_;
    cul::ts::set_context(suite, [](TestSuite & suite, Unit & unit) {
        EntryMakerMaker emm;
        auto a = emm.add_entry().set_bounds(Rectangle{ 0, 0, 10, 10 }).set_layer(k_sensor)();
        auto b = emm.add_entry().set_bounds(Rectangle{ 2, 2,  3,  3 }).set_layer(k_solid )();
        bool a_hit = false;
        auto event_handler = make_trespass_checker(emm,
            [&a_hit, &emm, &a](Entry lhs, Entry rhs, EventOccurrenceType)
        {
            a_hit =    emm.get_name(lhs.entity) == emm.get_name(a.entity)
                    || emm.get_name(rhs.entity) == emm.get_name(a.entity);
        });
        // one frame with 'a'
        auto handler = make_handler();
        unit.start(mark(suite), [&] {
            handler->update_entry(a);
            handler->run(event_handler);
            handler->update_entry(a);
            handler->update_entry(b);
            handler->run(event_handler);
            return test(a_hit);
        });
        unit.start(mark(suite), [&] {
            handler->update_entry(a);
            handler->update_entry(b);
            handler->run(event_handler);
            a_hit = false;

            handler->update_entry(a);
            handler->update_entry(b);
            handler->run(event_handler);
            return test(!a_hit);
        });
    });
}

void do_head_on_collision(TestSuite & suite, HandlerMaker make_handler) {
    // things we know
    // collision works on quadratic, not on sweep on demo
    // this case fails to reproduce it
    // no behavioral difference between native and WASM has been observed
    //
    // could this be attributed to "floating point madness"?
    // I tried compiling between float and double and got different behavior
    // for the "Head on Collision 2" scene. Double type emits no event, whereas
    // float does. I suggest this does prove "floating point madness".
    // However: the difference in behavior between quadratic and sweep is not
    // affected by this choice.
    //
    // There maybe two bugs here at play, one which may also concern method of
    // iteration
    static auto make_first_frame = [] (EntryMakerMaker & emm) {
        auto to_real = [](std::size_t sz)
            { return *reinterpret_cast<const Real *>(&sz); };
        const auto a_dx = to_real(13833556855406373547ull);
        const auto a_dy = to_real(13788701003117763407ull);
        const auto a_x  = to_real(4644278475105675910ull);
        const auto a_y  = to_real(4636724390418203728ull);
        const auto b_dx = to_real(4610184818551597739ull);
        const auto b_dy = to_real(4565328966262987599ull);
        const auto b_x  = to_real(4643692068904195407ull);
        const auto b_y  = to_real(4636750192291068848ull);

        return make_tuple(
            emm.add_entry().set_bounds(Rectangle{ a_x, a_y, 30, 30 })
                        .set_displacement(Vector{ a_dx, a_dy})
                        .set_layer(k_solid)(),
            emm.add_entry().set_bounds(Rectangle{ b_x, b_y, 30, 30 })
                        .set_displacement(Vector{ b_dx, b_dy})
                        .set_layer(k_solid)());

    };
    mark(suite).test([&] {
        EntryMakerMaker emm;
        const auto & [a, b] = make_first_frame(emm);
        auto event_handler = make_null_event_handler(emm);
        auto physics_handler = make_handler();
        physics_handler->update_entry(a);
        physics_handler->update_entry(b);

        physics_handler->run(event_handler);
        assert(!overlaps(a.bounds, b.bounds));
        physics_handler->update_entry(a);
        physics_handler->update_entry(b);

        physics_handler->run(event_handler);
        return test(!overlaps(a.bounds, b.bounds));
    });
}

void do_multiple_collision_bug(TestSuite & suite, HandlerMaker make_handler) {
    // successfully caught this bug
    // more information on the bug
    //
    // This process is sending a "rigid" collision immediately after the push
    //
    // This bug maybe fixed c:
    mark(suite).test([&] {
        EntryMakerMaker emm;
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{ 0, 0, 10, 10})
            .set_displacement(Vector{0, 1.5})
            .set_layer(k_solid)();
        const auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 0, 11, 10, 10})
            .make_pushable()
            .set_layer(k_solid)();
        const auto & c = emm.add_entry()
            .set_bounds(Rectangle{ 0, 22, 10, 10})
            .set_layer(k_solid)();
        int push_happened = 0;
        auto event_handler = make_collision_checker(emm,
            [&push_happened, &a, & b]
            (const Entry & lhs, const Entry & rhs, bool)
        {
            auto either_is = [&lhs, &rhs](const Entry & entry)
                { return lhs.entity == entry.entity || rhs.entity == entry.entity; };
            if (either_is(a) && either_is(b))
                ++push_happened;
        });
        auto physics_handler = make_handler();
        physics_handler->update_entry(a);
        physics_handler->update_entry(b);
        physics_handler->update_entry(c);
        physics_handler->run(event_handler);

        physics_handler->update_entry(a);
        physics_handler->update_entry(b);
        physics_handler->update_entry(c);
        physics_handler->run(event_handler);
        return test(push_happened == 1);
    });
}

void do_bad_entity_reference(TestSuite & suite, HandlerMaker make_handler) {
    // It's not possible to write this test with void pointer...
    //
    // It maybe possible to do it with shared_ptr.
    // 1.) Add the object entity
    // 2.) Delete the object's shared resource
    // 3.) Cause an event to be sent with the deleted resource
    // Deletion was likely to occur while "run" is being called by the event
    // handler.
    // It's even more baffling, the first part of the resource is being
    // deleted. (This doesn't reveal much tbh)
    mark(suite).test([&] {
        EntryMakerMaker emm;
        auto physics_handler = make_handler();
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{ 0, 0, 10, 10 })
            .set_displacement(Vector{12, 0})
            .set_layer(k_solid)();
        const auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 11, 0, 10, 10 })
            .set_layer(k_sensor)();
        const auto & c = emm.add_entry()
            .set_bounds(Rectangle{ 22, 0, 10, 10 })
            .set_layer(k_sensor)();
        bool trespass_b = false, trespass_c = false;
        auto event_handler = make_trespass_checker(emm,
            [&a, &b, &c, &trespass_b, &trespass_c]
            (const Entry & lhs, const Entry & rhs, EventOccurrenceType)
        {
            auto either_is = [&lhs, &rhs](const Entry & entry)
                { return lhs.entity == entry.entity || rhs.entity == entry.entity; };
            if (either_is(a) && either_is(b)) trespass_b = true;
            if (either_is(a) && either_is(c)) trespass_c = true;
        });
        for (auto * entry : { &a, &b, &c })
            physics_handler->update_entry(*entry);
        physics_handler->run(event_handler);
        physics_handler->remove_entry(c.entity);
        for (auto * entry : { &a, &b })
            physics_handler->update_entry(*entry);
        physics_handler->run(event_handler);
        return test(trespass_b && !trespass_c);
    });
}

void do_trespass_recording(TestSuite & suite, HandlerMaker make_handler) {
    // pass through trespass
    using namespace occurence_types;
    static auto make_otype_counter = []
        (EntryMakerMaker & emm, int & started, int & continued, int & ended)
    {
        return make_trespass_checker(emm,
            [&started, &continued, &ended](const Entry &, const Entry &, EventOccurrenceType otype)
        {
            using namespace occurence_types;
            switch (otype) {
            case k_on_begin   : ++started  ; break;
            case k_on_continue: ++continued; break;
            case k_on_end     : ++ended    ; break;
            default: break;
            }
        });
    };
    mark(suite).test([&] {
        EntryMakerMaker emm;
        auto handler = make_handler();
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{0,0, 10, 10})
            .set_displacement(Vector{12, 0})
            .set_layer(k_solid)();
        const auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 11, 0, 10, 10 })
            .set_layer(k_sensor)();
        handler->update_entry(a);
        handler->update_entry(b);
        handler->set_event_occurence_preference(k_on_begin | k_on_continue);

        int started = 0, continued = 0, ended = 0;
        auto event_handler = make_otype_counter(emm, started, continued, ended);
        handler->run(event_handler);
        handler->run(event_handler);
        handler->run(event_handler);
        return test(started == 1 && continued == 1 && ended == 0);
    });
    // on frame trespass
    mark(suite).test([&] {
        EntryMakerMaker emm;
        auto handler = make_handler();
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{0,0, 10, 10})
            .set_layer(k_solid)();
        const auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 5, 0, 10, 10 })
            .set_layer(k_sensor)();
        handler->update_entry(a);
        handler->update_entry(b);
        handler->set_event_occurence_preference(k_on_begin | k_on_continue);
        int started = 0, continued = 0, ended = 0;
        auto event_handler = make_otype_counter(emm, started, continued, ended);
        for (int i = 0; i != 5; ++i)
            handler->run(event_handler);
        return test(started == 1 && continued == 4 && ended == 0);
    });

    mark(suite).test([&] {
        EntryMakerMaker emm;
        auto handler = make_handler();
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{0,0, 10, 10})
            .set_layer(k_solid)();
        auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 5, 0, 10, 10 })
            .set_layer(k_sensor)();
        handler->update_entry(a);
        handler->update_entry(b);
        handler->set_event_occurence_preference(k_on_begin | k_on_continue | k_on_end);
        int started = 0, continued = 0, ended = 0;
        auto event_handler = make_otype_counter(emm, started, continued, ended);
        for (int i = 0; i != 4; ++i)
            handler->run(event_handler);
        b.displacement = Vector{6, 0};
        assert(!overlaps(a.bounds, displace(b.bounds, b.displacement)));
        // note: b will trespass for the next frame, but not the one after
        //       this because b's bounds still overlaps at the beginning of the
        //       frame
        handler->update_entry(b);
        for (int i = 0; i != 3; ++i)
            handler->run(event_handler);

        return test(started == 1 && continued == 4 && ended == 1);
    });
    mark(suite).test([&] {
        EntryMakerMaker emm;
        auto handler = make_handler();
        const auto & a = emm.add_entry()
            .set_bounds(Rectangle{0,0, 10, 10})
            .set_layer(k_solid)();
        auto & b = emm.add_entry()
            .set_bounds(Rectangle{ 5, 0, 10, 10 })
            .set_layer(k_sensor)();
        handler->update_entry(a);
        handler->update_entry(b);
        handler->set_event_occurence_preference(k_on_begin | k_on_end);
        int started = 0, continued = 0, ended = 0;
        auto event_handler = make_otype_counter(emm, started, continued, ended);
        for (int i = 0; i != 4; ++i)
            handler->run(event_handler);
        b.displacement = Vector{6, 0};
        handler->update_entry(b);
        for (int i = 0; i != 3; ++i)
            handler->run(event_handler);

        return test(started == 1 && continued == 0 && ended == 1);
    });
}
