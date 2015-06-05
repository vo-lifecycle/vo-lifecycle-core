package org.volifecycle.tests.lifecycle;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChecker;
import org.volifecycle.lifecycle.impl.LifeCycleTransitionImpl;
import org.volifecycle.tests.AbstractTest;
import org.volifecycle.tests.inputs.ValueObjectStub;

/**
 * LifeCycleTransitionImpl tests
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
@RunWith(MockitoJUnitRunner.class)
public class LifeCycleTransitionImplTest extends AbstractTest {
    /**
     * Mocks
     */
    @Mock
    LifeCycleChecker<ValueObjectStub> checkerMock;

    @Mock
    LifeCycleAdapter<ValueObjectStub> adapterMock;

    @Mock
    EventManager evtManagerMock;

    LifeCycleTransitionImpl<ValueObjectStub> transition;
    ValueObjectStub valueObject;
    List<LifeCycleChecker<ValueObjectStub>> lstCheckers;
    String idChecker = "ID";
    List<String> forcedCheckers;
    String targetState = "STATE";

    /**
     * Init datas
     */
    @Before
    public final void initData() {
        valueObject = new ValueObjectStub();
        transition = new LifeCycleTransitionImpl<ValueObjectStub>();

        lstCheckers = new ArrayList<LifeCycleChecker<ValueObjectStub>>();
        lstCheckers.add(checkerMock);
        transition.setCheckers(lstCheckers);

        forcedCheckers = new ArrayList<String>();
        forcedCheckers.add(idChecker);

        // mocks configuration
        String rtn[] = new String[2];
        rtn[0] = LifeCycleConstants.FALSE;
        when(checkerMock.getResult(any(ValueObjectStub.class))).thenReturn(rtn);
        when(checkerMock.getId()).thenReturn(idChecker);
        when(checkerMock.getTargetState()).thenReturn(targetState);

        when(adapterMock.getState(any(ValueObjectStub.class))).thenReturn(valueObject.getState());

        when(adapterMock.getType(any(ValueObjectStub.class))).thenReturn(valueObject.getType());
    }

    /**
     * Change state nominal
     */
    @Test
    public final void testChangeStateNominal() {
        String result = transition.changeState(valueObject, adapterMock, evtManagerMock);
        assertEquals(LifeCycleConstants.FALSE, result);
    }

    /**
     * Change state with forced checker
     */
    @Test
    public final void testChangeStateWithForcedChecker() {
        String result = transition.changeState(valueObject, adapterMock, evtManagerMock, forcedCheckers);
        assertEquals(targetState, result);
        verify(evtManagerMock).logEvent(any(Event.class));
    }
}
