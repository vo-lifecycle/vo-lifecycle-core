package org.volifecycle.tests.lifecycle;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChecker;
import org.volifecycle.lifecycle.impl.LifeCycleCheckerImpl;
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

	LifeCycleTransitionImpl<ValueObjectStub> transition;
	LifeCycleCheckerImpl<ValueObjectStub> checker;

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
		checker = new LifeCycleCheckerImpl<ValueObjectStub>() {
		};

		lstCheckers = new ArrayList<LifeCycleChecker<ValueObjectStub>>();
		lstCheckers.add(checker);
		transition.setCheckers(lstCheckers);

		forcedCheckers = new ArrayList<String>();
		forcedCheckers.add(idChecker);

		checker.setId(idChecker);
		checker.setTargetState(targetState);

		// mocks configuration
		when(adapterMock.getState(any(ValueObjectStub.class))).thenReturn(valueObject.getState());
		when(adapterMock.getType(any(ValueObjectStub.class))).thenReturn(valueObject.getType());
	}

	/**
	 * Change state nominal
	 */
	@Test
	@Ignore
	// FIXME à mettre à jour
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
	}
}
