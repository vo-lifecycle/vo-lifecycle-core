package org.volifecycle.tests.event;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    List<String> propertyFilters;
    List<String> classFilters;

    /**
     * Init datas
     */
    @Before
    public final void initData() {
        detector = new DiffDetectorImpl<ValueObjectStub, LifeCycleAdapter<ValueObjectStub>>();
        detector.setAdapter(adapterMock);
        detector.setEvtManager(evtManagerMock);

        propertyFilters = new ArrayList<String>();
        propertyFilters.add("id");
        propertyFilters.add("label");
        propertyFilters.add("subValueObject");
        propertyFilters.add("map");
        propertyFilters.add("nb");
        propertyFilters.add("lstChilds");

        detector.setPropertyFilters(propertyFilters);

        classFilters = new ArrayList<String>();
        classFilters.add("org.volifecycle.tests.inputs.ValueObjectStub");
        classFilters.add("org.volifecycle.tests.inputs.SubValueObject");

        detector.setClassFilters(classFilters);
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
     * Simple test detector
     */
    @Test
    public final void testDetectorWithPropertyFilterEmpty() {
        ValueObjectStub vo1 = new ValueObjectStub();
        vo1.setId("1");

        ValueObjectStub vo2 = new ValueObjectStub();
        vo2.setId("2");

        detector.setPropertyFilters(null);
        detector.compare(vo1, vo2);
        verify(evtManagerMock, times(0)).logEvent(any(Event.class));
    }

    /**
     * Simple test detector
     */
    @Test
    public final void testDetectorWithClassFilterEmpty() {
        ValueObjectStub vo1 = new ValueObjectStub();
        vo1.setId("1");

        ValueObjectStub vo2 = new ValueObjectStub();
        vo2.setId("2");

        detector.setClassFilters(null);
        detector.compare(vo1, vo2);
        verify(evtManagerMock, times(0)).logEvent(any(Event.class));
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

    /**
     * Test with subobject list - same size
     */
    @Test
    public final void testDetectorSubObjectListSameSize() {
        SubValueObject so1 = new SubValueObject();
        so1.setCode("123");
        so1.setLabel("label1");
        List<SubValueObject> l1 = new ArrayList<SubValueObject>();
        l1.add(so1);

        SubValueObject so2 = new SubValueObject();
        so2.setCode("123");
        so2.setLabel("label2");
        List<SubValueObject> l2 = new ArrayList<SubValueObject>();
        l2.add(so2);

        ValueObjectStub vo1 = new ValueObjectStub();
        vo1.setLstChilds(l1);

        ValueObjectStub vo2 = new ValueObjectStub();
        vo2.setLstChilds(l2);

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
        assertEquals("lstChilds", d.getDiffProperties().get(0).getParentPropertyName());
    }

    /**
     * Test with subobject list - not same size
     */
    @Test
    public final void testDetectorSubObjectListNotSameSize() {
        SubValueObject so1 = new SubValueObject();
        so1.setCode("123");
        so1.setLabel("label1");
        List<SubValueObject> l1 = new ArrayList<SubValueObject>();
        l1.add(so1);

        SubValueObject so2 = new SubValueObject();
        so2.setCode("123");
        so2.setLabel("label2");
        List<SubValueObject> l2 = new ArrayList<SubValueObject>();
        l2.add(so2);
        l2.add(so1);

        ValueObjectStub vo1 = new ValueObjectStub();
        vo1.setLstChilds(l1);

        ValueObjectStub vo2 = new ValueObjectStub();
        vo2.setLstChilds(l2);

        detector.compare(vo1, vo2);
        verify(evtManagerMock).logEvent(captorEvent.capture());
        Event result = captorEvent.getValue();

        assertNotNull(result);
        assertTrue(result instanceof DiffEvent);

        DiffEvent d = (DiffEvent) result;
        assertNotNull(d.getDiffProperties());
        assertEquals(3, d.getDiffProperties().size());

        assertEquals("1", d.getDiffProperties().get(0).getBeforeValue());
        assertEquals("2", d.getDiffProperties().get(0).getAfterValue());
        assertEquals("lstChilds", d.getDiffProperties().get(0).getPropertyName());
        assertEquals(LifeCycleConstants.DIFF_TYPE_SIZE, d.getDiffProperties().get(0).getType());
        assertNull(d.getDiffProperties().get(0).getParentPropertyName());

        // Object added
        assertEquals("null", d.getDiffProperties().get(1).getBeforeValue());
        assertEquals("label1", d.getDiffProperties().get(1).getAfterValue());
        assertEquals("lstChilds", d.getDiffProperties().get(1).getParentPropertyName());
        assertEquals("label", d.getDiffProperties().get(1).getPropertyName());
        assertEquals(LifeCycleConstants.DIFF_TYPE_VALUE, d.getDiffProperties().get(1).getType());

        // Object diff
        assertEquals("label1", d.getDiffProperties().get(2).getBeforeValue());
        assertEquals("label2", d.getDiffProperties().get(2).getAfterValue());
        assertEquals("lstChilds", d.getDiffProperties().get(2).getParentPropertyName());
        assertEquals("label", d.getDiffProperties().get(2).getPropertyName());
        assertEquals(LifeCycleConstants.DIFF_TYPE_VALUE, d.getDiffProperties().get(2).getType());
    }

    /**
     * Test not implemented type
     */
    @Test
    public final void testDetectorSubObjectListNotImplementedType() {
        ValueObjectStub vo1 = new ValueObjectStub();
        ValueObjectStub vo2 = new ValueObjectStub();

        Map<String, String> m1 = new HashMap<String, String>();
        m1.put("1", "2");

        Map<String, String> m2 = new HashMap<String, String>();
        m2.put("2", "2");

        vo1.setMap(m1);
        vo2.setMap(m2);
        detector.compare(vo1, vo2);
        verify(evtManagerMock, times(0)).logEvent(any(Event.class));

    }
}
