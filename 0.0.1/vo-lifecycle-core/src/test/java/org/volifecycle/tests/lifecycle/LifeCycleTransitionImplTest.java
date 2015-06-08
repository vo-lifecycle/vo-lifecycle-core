package org.volifecycle.tests.lifecycle;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyMapOf;
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
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.impl.LifeCycleCompositeActionImpl;
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
	LifeCycleAdapter<ValueObjectStub> adapterMock;

	@Mock
	EventManager evtManagerMock;

	@Mock
	LifeCycleAction<ValueObjectStub> actionMock;

	LifeCycleTransitionImpl<ValueObjectStub> transition;
	LifeCycleCompositeActionImpl<ValueObjectStub> action;

	ValueObjectStub valueObject;
	List<LifeCycleAction<ValueObjectStub>> lstActions;
	String idAction = "ID";
	List<String> forcedActions;
	String targetState = "STATE";

	/**
	 * Init datas
	 */
	@Before
	public final void initData() {
		valueObject = new ValueObjectStub();
		transition = new LifeCycleTransitionImpl<ValueObjectStub>();
		action = new LifeCycleCompositeActionImpl<ValueObjectStub>() {
		};

		lstActions = new ArrayList<LifeCycleAction<ValueObjectStub>>();
		lstActions.add(action);
		transition.setActions(lstActions);

		forcedActions = new ArrayList<String>();
		forcedActions.add(idAction);

		List<LifeCycleAction<ValueObjectStub>> simpleActions = new ArrayList<LifeCycleAction<ValueObjectStub>>();
		simpleActions.add(actionMock);

		action.setId(idAction);
		action.setTargetState(targetState);
		action.setSimpleActions(simpleActions);

		List<String> targetStates = new ArrayList<String>();
		targetStates.add(targetState);
		transition.setTargetStates(targetStates);

		// mocks configuration
		when(adapterMock.getState(any(ValueObjectStub.class))).thenReturn(valueObject.getState());
		when(adapterMock.getType(any(ValueObjectStub.class))).thenReturn(valueObject.getType());
		when(actionMock.getId()).thenReturn(idAction);
	}

	/**
	 * Change state nominal
	 */
	@Test
	public final void testChangeStateNominal() {
		when(actionMock.getResult(any(ValueObjectStub.class), anyMapOf(String.class, Object.class))).thenReturn(targetState);
		String result = transition.changeState(valueObject, adapterMock, evtManagerMock);
		assertEquals(targetState, result);
	}

	/**
	 * Change state failed
	 */
	@Test
	public final void testChangeStateFailed() {
		when(actionMock.getResult(any(ValueObjectStub.class), anyMapOf(String.class, Object.class))).thenReturn(LifeCycleConstants.FALSE);
		String result = transition.changeState(valueObject, adapterMock, evtManagerMock);
		assertEquals(LifeCycleConstants.FALSE, result);
	}

	/**
	 * Change state with forced action
	 */
	@Test
	public final void testChangeStateWithForcedAction() {
		when(actionMock.getResult(any(ValueObjectStub.class), anyMapOf(String.class, Object.class))).thenReturn(LifeCycleConstants.FALSE);
		String result = transition.changeState(valueObject, adapterMock, evtManagerMock, forcedActions);
		assertEquals(targetState, result);
	}
}
