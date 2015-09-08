package org.volifecycle.tests.lifecycle;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.extra.jsr303.LifeCycleJSR303ActionImpl;
import org.volifecycle.tests.AbstractTest;
import org.volifecycle.tests.inputs.ValueObjectStub;

/**
 * Tests of LifeCycleJSR303ActionImpl class.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
@RunWith(MockitoJUnitRunner.class)
public class LifeCycleJSR303ActionImplTest extends AbstractTest {
    @Mock
    LifeCycleAdapter<ValueObjectStub> adapterMock;

    @Mock
    EventManager evtManagerMock;

    LifeCycleJSR303ActionImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>> action;

    ValueObjectStub valueObject;

    /**
     * Initializing test's datas.
     */
    @Before
    public final void init() {
        action = new LifeCycleJSR303ActionImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>>();
        valueObject = new ValueObjectStub();
        action.setEvtManager(evtManagerMock);
        action.setAdapter(adapterMock);
    }

    /**
     * Nominal test
     */
    @Test
    public final void testValidationNominal() {
        // required field
        valueObject.setNb(1F);

        assertEquals(Boolean.TRUE.toString(), action.getResult(valueObject, null));
    }

    /**
     * Failure test
     */
    @Test
    public final void testValidationFailure() {
        // required field
        valueObject.setNb(null);

        assertEquals(Boolean.FALSE.toString(), action.getResult(valueObject, null));
    }
}
