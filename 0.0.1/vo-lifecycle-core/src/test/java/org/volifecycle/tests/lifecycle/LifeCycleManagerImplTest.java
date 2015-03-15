package org.volifecycle.tests.lifecycle;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyListOf;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChangeSaver;
import org.volifecycle.lifecycle.LifeCycleConstants;
import org.volifecycle.lifecycle.LifeCycleState;
import org.volifecycle.lifecycle.LifeCycleTransition;
import org.volifecycle.lifecycle.impl.LifeCycleManagerImpl;
import org.volifecycle.lifecycle.vo.LifeCycleChange;
import org.volifecycle.tests.AbstractTest;
import org.volifecycle.tests.inputs.ValueObjectStub;

/**
 * LifeCycleManagerImpl tests
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
@RunWith(MockitoJUnitRunner.class)
public class LifeCycleManagerImplTest extends AbstractTest {
	LifeCycleManagerImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>> manager;
	Map<String, LifeCycleState<ValueObjectStub>> statesById;
	Map<String, LifeCycleTransition<ValueObjectStub>> transitionsById;

	@Mock
	LifeCycleState<ValueObjectStub> stateMock;

	@Mock
	LifeCycleTransition<ValueObjectStub> transitionMock;

	@Mock
	LifeCycleAdapter<ValueObjectStub> adapterMock;

	@Mock
	EventManager evtManagerMock;

	@Mock
	LifeCycleChangeSaver saverMock;

	String transitionId = "id1";
	String stateId = "idState";

	ValueObjectStub valueObject;

	/**
	 * Init data
	 */
	@Before
	public final void initData() {
		valueObject = new ValueObjectStub();
		manager = new LifeCycleManagerImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>>();
		manager.setAdapter(adapterMock);
		manager.setEvtManager(evtManagerMock);
		manager.setSaver(saverMock);

		statesById = new HashMap<String, LifeCycleState<ValueObjectStub>>();
		statesById.put(stateId, stateMock);
		manager.setStatesById(statesById);

		transitionsById = new HashMap<String, LifeCycleTransition<ValueObjectStub>>();
		transitionsById.put(transitionId, transitionMock);

		when(stateMock.getTransitionsById()).thenReturn(transitionsById);
		when(
				transitionMock.changeState(eq(valueObject), eq(adapterMock),
						eq(evtManagerMock), anyListOf(String.class)))
				.thenReturn(LifeCycleConstants.TRUE);
	}

	/**
	 * Test function logChangeCustom
	 */
	@Test
	public final void testLogChangeCustomNominal() {
		manager.logChangeCustom(valueObject, transitionId, "satein",
				adapterMock, "state-out");
		verify(adapterMock).getId(valueObject);
		verify(adapterMock).getType(valueObject);
		verify(saverMock).logChange(any(LifeCycleChange.class));
	}

	/**
	 * Run existing transition
	 */
	@Test
	public final void testRunTransitionNominal() {
		when(adapterMock.getState(valueObject)).thenReturn(stateId);
		assertEquals(LifeCycleConstants.TRUE,
				manager.runTransition(transitionId, valueObject));
	}

	/**
	 * Run existing transition with unknown value object state
	 */
	@Test(expected = IllegalStateException.class)
	public final void testRunTransitionUnknownState() {
		manager.runTransition(transitionId, valueObject);
	}

	/**
	 * Run unknown transition
	 */
	@Test(expected = IllegalStateException.class)
	public final void testRunTransitionUnknownTransition() {
		when(adapterMock.getState(valueObject)).thenReturn(stateId);
		manager.runTransition("unknown", valueObject);
	}

	/**
	 * Null value object test
	 */
	@Test(expected = IllegalStateException.class)
	public final void testRunTransitionNullVO() {
		when(adapterMock.getState(valueObject)).thenReturn(stateId);
		manager.runTransition(transitionId, null);
	}
}
