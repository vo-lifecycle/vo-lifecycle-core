package org.volifecycle.tests.event;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.DiffDetectorImpl;
import org.volifecycle.event.vo.DiffEvent;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.tests.inputs.SubValueObject;
import org.volifecycle.tests.inputs.ValueObjectStub;

/**
 * Test of DiffDetectorImpl class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
@RunWith(MockitoJUnitRunner.class)
public class DiffDetectorImplTest {
	/**
	 * Mocks
	 */
	@Mock
	LifeCycleAdapter<ValueObjectStub> adapterMock;

	@Mock
	EventManager evtManagerMock;

	@Captor
	ArgumentCaptor<Event> captorEvent;

	DiffDetectorImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>> detector;

	/**
	 * Init datas
	 */
	@Before
	public final void initData() {
		detector = new DiffDetectorImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>>();
		detector.setAdapter(adapterMock);
		detector.setEvtManager(evtManagerMock);
	}

	/**
	 * Simple test detector
	 */
	@Test
	public final void testDetectorNominal() {
		ValueObjectStub vo1 = new ValueObjectStub();
		vo1.setId("1");

		ValueObjectStub vo2 = new ValueObjectStub();
		vo2.setId("2");

		detector.compare(vo1, vo2);
		verify(evtManagerMock).logEvent(captorEvent.capture());
		Event result = captorEvent.getValue();

		assertNotNull(result);
		assertTrue(result instanceof DiffEvent);

		DiffEvent d = (DiffEvent) result;
		assertNotNull(d.getDiffProperties());
		assertEquals(1, d.getDiffProperties().size());
		assertEquals("1", d.getDiffProperties().get(0).getBeforeValue());
		assertEquals("2", d.getDiffProperties().get(0).getAfterValue());
		assertEquals("id", d.getDiffProperties().get(0).getPropertyName());
		assertEquals(LifeCycleConstants.DIFF_TYPE_VALUE, d.getDiffProperties().get(0).getType());
		assertNull(d.getDiffProperties().get(0).getParentPropertyName());
	}

	/**
	 * Test with float
	 */
	@Test
	public final void testDetectorFloat() {
		ValueObjectStub vo1 = new ValueObjectStub();
		vo1.setNb(1.2f);

		ValueObjectStub vo2 = new ValueObjectStub();
		vo2.setNb(2.3f);

		detector.compare(vo1, vo2);
		verify(evtManagerMock).logEvent(captorEvent.capture());
		Event result = captorEvent.getValue();

		assertNotNull(result);
		assertTrue(result instanceof DiffEvent);

		DiffEvent d = (DiffEvent) result;
		assertNotNull(d.getDiffProperties());
		assertEquals(1, d.getDiffProperties().size());
		assertEquals("1.2", d.getDiffProperties().get(0).getBeforeValue());
		assertEquals("2.3", d.getDiffProperties().get(0).getAfterValue());
		assertEquals("nb", d.getDiffProperties().get(0).getPropertyName());
		assertEquals(LifeCycleConstants.DIFF_TYPE_VALUE, d.getDiffProperties().get(0).getType());
		assertNull(d.getDiffProperties().get(0).getParentPropertyName());
	}

	/**
	 * Test with subobject
	 */
	@Test
	public final void testDetectorSubObject() {
		SubValueObject so1 = new SubValueObject();
		so1.setCode("123");
		so1.setLabel("label1");

		SubValueObject so2 = new SubValueObject();
		so2.setCode("123");
		so2.setLabel("label2");

		ValueObjectStub vo1 = new ValueObjectStub();
		vo1.setSubValueObject(so1);

		ValueObjectStub vo2 = new ValueObjectStub();
		vo2.setSubValueObject(so2);

		detector.compare(vo1, vo2);
		verify(evtManagerMock).logEvent(captorEvent.capture());
		Event result = captorEvent.getValue();

		assertNotNull(result);
		assertTrue(result instanceof DiffEvent);

		DiffEvent d = (DiffEvent) result;
		assertNotNull(d.getDiffProperties());
		assertEquals(1, d.getDiffProperties().size());
		assertEquals("label1", d.getDiffProperties().get(0).getBeforeValue());
		assertEquals("label2", d.getDiffProperties().get(0).getAfterValue());
		assertEquals("label", d.getDiffProperties().get(0).getPropertyName());
		assertEquals(LifeCycleConstants.DIFF_TYPE_VALUE, d.getDiffProperties().get(0).getType());
		assertEquals("subValueObject", d.getDiffProperties().get(0).getParentPropertyName());
	}
}
