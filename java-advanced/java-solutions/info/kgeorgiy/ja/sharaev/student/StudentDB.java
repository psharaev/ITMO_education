package info.kgeorgiy.ja.sharaev.student;

import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

public class StudentDB implements StudentQuery {

    private static final Comparator<Student> NAME_ORDER =
            Comparator.comparing(Student::getLastName)
                    .thenComparing(Student::getFirstName)
                    .reversed()
                    .thenComparingInt(Student::getId);

    //region get

    private <R> List<R> mapStudents(final List<Student> students, final Function<Student, R> mapper) {
        return students.stream().map(mapper).toList();
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return mapStudents(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return mapStudents(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(final List<Student> students) {
        return mapStudents(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return mapStudents(students, student -> student.getFirstName() + " " + student.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return students.stream().map(Student::getFirstName).collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(final List<Student> students) {
        return students.stream().max(Student::compareTo).map(Student::getFirstName).orElse("");
    }

    //endregion
    //region sort

    private List<Student> sortStudents(final Collection<Student> students, final Comparator<Student> cmp) {
        return students.stream().sorted(cmp).toList();
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return sortStudents(students, Comparator.comparingInt(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return sortStudents(students, NAME_ORDER);
    }

    //endregion
    //region find

    private static <T> List<Student> filterStudents(final Collection<Student> students, final Function<Student, T> fn, final T key) {
        return students.stream().filter(s -> key.equals(fn.apply(s))).sorted(NAME_ORDER).toList();
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, final String name) {
        return filterStudents(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, final String name) {
        return filterStudents(students, Student::getLastName, name);
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, final GroupName group) {
        return filterStudents(students, Student::getGroup, group);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, final GroupName group) {
        return students.stream()
                .filter(student -> student.getGroup().equals(group))
                .collect(Collectors.toMap(
                        Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(String::compareTo)
                ));
    }

    //endregion
}
